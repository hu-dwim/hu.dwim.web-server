;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server.test)

(def special-variable *parameter-application* (make-instance 'standard-application :path-prefix "/parameter/"))

(def function render-mime-part-details (mime-part)
  <p "Mime part headers:">
  <table
    <thead <tr <th "Header">
               <th "Value">>>
    ,@(iter (for header :in (rfc2388-binary:headers mime-part))
            (collect <tr
                      <td ,(or (rfc2388-binary:header-name header) "")>
                      <td ,(or (rfc2388-binary:header-value header) "")>>))>
  <table
    <thead <tr <th "Property">
               <th "Value">>>
    <tr <td "Content">
        <td ,(princ-to-string (rfc2388-binary:content mime-part))>>
    <tr <td "Content charset">
        <td ,(rfc2388-binary:content-charset mime-part)>>
    <tr <td "Content length">
        <td ,(rfc2388-binary:content-length mime-part)>>
    <tr <td "Content type">
        <td ,(rfc2388-binary:content-type mime-part)>>>)

(def entry-point (*parameter-application* :path "")
  (with-request-parameters ((number "0" number?) ((the-answer "theanswer") "not supplied" the-answer?))
    (declare (ignore number the-answer))
    (make-raw-functional-response ()
      (emit-http-response/simple-html-document (:title "foo")
        <p "Parameters:"
          <a (:href ,(string+ (path-prefix-of *parameter-application*)
                              (unless (or number? the-answer?)
                                "?theanswer=yes&number=42")))
            "try this">>
        <table
          <thead <tr <td "Parameter name">
                     <td "Parameter value">>>
          ,@(do-parameters (name value)
              <tr
                <td ,name>
                <td ,(etypecase value
                       (string value)
                       (list (princ-to-string value))
                       (rfc2388-binary:mime-part
                        (render-mime-part-details value)))>>)>
        <hr>
        <form (:method "post")
          <input (:name "input1"             :value ,(or (parameter-value "input1") "1"))>
          <input (:name "input2-with-áccent" :value ,(or (parameter-value "input2-with-áccent") "Ááő\"$&+ ?űúéö"))>
          <input (:type "submit")>>
        <form (:method "post" :enctype "multipart/form-data")
          <input (:name "file-input1" :type "file")>
          <input (:name "file-input2" :value ,(or (parameter-value "file-input2") "file2"))>
          <input (:type "submit")>>
        <hr>
        <p "The parsed request: ">
        (render-request *request*)))))
