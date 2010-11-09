;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; Util

(def localization en
  (sequence.empty "empty")
  (sequence.element "element"))

;;;;;;
;;; Mime type

(def localization en
  (mime-type.application/msword "Microsoft Word Document")
  (mime-type.application/vnd.ms-excel "Microsoft Excel Document")
  (mime-type.application/pdf "PDF Document")
  (mime-type.image/png "PNG image")
  (mime-type.image/tiff "TIFF image"))

;;;;;;
;;; Error handling

(def localization en
  (error.internal-server-error "Internal server error")
  (error.access-denied-error "Access denied")

  (render-error-page/internal-error (&key administrator-email-address &allow-other-keys)
    <div
      <h1 "Internal server error">
      <p "An internal server error has occurred while processing your request. We are sorry for the inconvenience.">
      <p "The developers will be notified about this error and will hopefully fix it in the near future.">
      ,(when administrator-email-address
         <p "You may contact the administrators at the "
            <a (:href ,(mailto-href administrator-email-address)) ,administrator-email-address>
            " email address.">)
      <p <a (:href "#" :onClick `js-inline(history.go -1)) "Go back">>>)

  (render-error-page/access-denied (&key &allow-other-keys)
    <div
      <h1 "Access denied">
      <p "You have no permission to access the requested resource.">
      <p <a (:href "#" :onClick `js-inline(history.go -1)) "Go back">>>))
