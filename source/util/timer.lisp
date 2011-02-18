(in-package :hu.dwim.web-server)

;;;;;;
;;; A simple task scheduler

(def class* timer ()
  ((entries nil)
   (lock (make-recursive-lock "Timer lock"))
   (running-thread nil)
   (shutdown-initiated #f :type boolean)
   (condition-variable (make-condition-variable))))

(def with-macro with-lock-held-on-timer (timer)
  (with-lock-held-on-thing ('timer timer)
    (-with-macro/body-)))

(def function drive-timer (timer)
  "This is the entry point of the timer. There should be one and only one thread calling this function for each timer."
  (timer.debug "Thread is entering the timer loop DRIVE-TIMER of ~A" timer)
  (unwind-protect
       (progn
         (assert (null (running-thread-of timer)))
         (setf (running-thread-of timer) (current-thread))
         (setf (shutdown-initiated-p timer) #f)
         (with-simple-restart (abort-timer "Break out from the timer loop")
           (loop
             :until (shutdown-initiated-p timer)
             :do (drive-timer/process-entries timer))))
    (setf (running-thread-of timer) nil)
    (with-lock-held-on-timer timer
      (condition-notify (condition-variable-of timer)))
    (timer.debug "Thread is leaving the timer loop DRIVE-TIMER of ~A" timer)))

(def function drive-timer/process-entries (timer)
  (flet ((reschedule-entries ()
           (setf (entries-of timer)
                 (sort (delete-if (complement #'timer-entry-valid?) (entries-of timer))
                       'local-time:timestamp<
                       :key 'run-at-of))))
    (timer.debug "Thread is entering the timer loop DRIVE-TIMER of ~A" timer)
    (bind ((entries)
           (run-anything? #f))
      (with-lock-held-on-timer timer
        (timer.dribble "~A sorting ~A entries" timer (length (entries-of timer)))
        ;; need to copy, because we will release the lock before processing finishes
        (setf entries (copy-list (reschedule-entries))))
      (dolist (entry entries)
        (when (local-time:timestamp< (run-at-of entry) (local-time:now))
          (setf run-anything? #t)
          (run-timer-entry timer entry)))
      (unless run-anything?
        (with-lock-held-on-timer timer
          (bind ((first-entry (first (reschedule-entries)))
                 (expires-in (or (when first-entry
                                   (local-time:timestamp-difference (run-at-of first-entry) (local-time:now)))
                                 ;; this is an ad-hoc large constant to keep the code path uniform. would be safe to wake up though...
                                 (* 60 60 24 365 10))))
            (timer.dribble "~A will fall asleep for ~A seconds" timer expires-in)
            (with-simple-restart (tick-timer "Wake up timer and process possible pending events")
              (handler-case
                  (with-deadline (expires-in)
                    (with-thread-name (string+ " / sleeping until next timer event (" (princ-to-string expires-in) " sec)")
                      (condition-wait (condition-variable-of timer) (lock-of timer)))
                    (timer.dribble "~A woke up from CONDITION-WAIT" timer))
                (deadline-timeout ()
                  (timer.dribble "~A woke up from CONDITION-WAIT due to the timeout" timer))))))))))

(def (function e) drive-timer/abort (timer)
  (if (eq (current-thread) (running-thread-of timer))
      (progn
        (invoke-restart 'abort-timer)
        (error "It should be impossible to get here..."))
      ;; register an instant entry which will call us in the timer thread
      (register-timer-entry timer
                            (named-lambda timer-aborter ()
                              (timer.debug "Calling DRIVE-TIMER/ABORT in the timer thread~%")
                              (drive-timer/abort timer))
                            :run-at (local-time:now)
                            :name "DRIVE-TIMER/ABORT message to the timer thread")))

(def function shutdown-timer (timer &key (wait #t))
  (if wait
      (loop
         :while (running-thread-of timer) :do
         (with-lock-held-on-timer timer
           (setf (shutdown-initiated-p timer) #t)
           (condition-wait (condition-variable-of timer) (lock-of timer))))
      (setf (shutdown-initiated-p timer) #t)))

(def (function e) register-timer-entry (timer thunk &key
                                              interval
                                              (run-at (when interval
                                                        (local-time:now)))
                                              (name "<unnamed>"))
  (check-type run-at local-time:timestamp)
  (check-type interval (or null number))
  (timer.debug "Registering timer entry ~S for timer ~A, at first time ~A, time interval ~A, thunk ~A" name timer run-at interval thunk)
  (with-lock-held-on-timer timer
    (push (if interval
              (make-instance 'periodic-timer-entry
                             :name name
                             :run-at run-at
                             :interval interval
                             :thunk thunk)
              (make-instance 'single-shot-timer-entry
                             :name name
                             :run-at run-at
                             :thunk thunk))
          (entries-of timer))
    (timer.debug "Waking up timer ~A because of a new entry" timer)
    (condition-notify (condition-variable-of timer))))

(def function timer-entry-valid? (entry)
  (not (null (thunk-of entry))))

(def class* timer-entry ()
  ((name (mandatory-argument) :type string)
   (run-at :type local-time:timestamp)
   (thunk (mandatory-argument) :type (or symbol function))))

(def print-object (timer-entry :identity #f)
  (prin1 (name-of -self-)))

(def class* single-shot-timer-entry (timer-entry)
  ())

(def class* periodic-timer-entry (timer-entry)
  ((interval (mandatory-argument))))

(def generic run-timer-entry (timer entry)
  (:method ((timer timer) (entry timer-entry))
    (timer.dribble "Running timer entry ~A" entry)
    (awhen (thunk-of entry)
      (block running-timer-entry
        (with-layered-error-handlers ((lambda (error)
                                        (handle-toplevel-error timer error))
                                      (lambda (&key &allow-other-keys)
                                        (return-from running-timer-entry)))
          (with-simple-restart (skip-timer-entry "Skip calling timer entry ~A" entry)
            (with-thread-name " / running timer entry"
              (funcall it)))))))

  (:method :after ((timer timer) (entry single-shot-timer-entry))
    (timer.debug "Invalidating single shot timer entry ~A" entry)
    (setf (thunk-of entry) nil))

  (:method :after ((timer timer) (entry periodic-timer-entry))
    (setf (run-at-of entry) (local-time:adjust-timestamp (local-time:now) (offset :sec (interval-of entry))))))
