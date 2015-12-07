;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.web-server.websocket
  :description "WebSocket extensions for hu.dwim.web-server."
  :author "Kambiz Darabi <kambiz.darabi@gmail.com>"
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:hu.dwim.web-server
               :cl-base64
               :ironclad)
  :components ((:module "source"
                        :components ((:module "server"
                                              ;; if we were not loading this in a separate asdf system: :depends-on ("variables" "brokers" "request-response")
                                              :components ((:file "websocket")))))))
