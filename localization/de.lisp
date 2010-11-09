;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; Util

(def localization de
  (sequence.empty "leer")
  (sequence.element "Element"))

;;;;;;
;;; Mime type

(def localization de
  (mime-type.application/msword "Microsoft Word Dokument")
  (mime-type.application/vnd.ms-excel "Microsoft Excel Dokument")
  (mime-type.application/pdf "PDF Dokument")
  (mime-type.image/png "PNG Bild")
  (mime-type.image/tiff "TIFF Bild"))

;;;;;;
;;; Error handling

(def localization de
  (error.internal-server-error "Interner Serverfehler")
  (error.access-denied-error "Zugriff verweigert")

  (render-error-page/internal-error (&key administrator-email-address &allow-other-keys)
    <div
      <h1 "Interner Serverfehler">
      <p "Ein Serverfehler ist bei der Bearbeitung Ihrer Anfrage aufgetreten. Wir bitten hierfür um Entschuldigung.">
      <p "Die Entwickler werden über diesen Fehler informiert und werden den Fehler in naher Zukunft beheben.">
      ,(when administrator-email-address
         <p "Sie können die Administratoren dieser Webseiten unter folgender Email-Adresse erreichen:"
            <a (:href ,(mailto-href administrator-email-address)) ,administrator-email-address>>)
      <p <a (:href "#" :onClick `js-inline(history.go -1)) "Zurück">>>)

  (render-error-page/access-denied (&key &allow-other-keys)
    <div
      <h1 "Zugriff verweigert">
      <p "Sie sind leider nicht berechtigt, auf die angeforderte Seite zu zu greifen.">
      <p <a (:href "#" :onClick `js-inline(history.go -1)) "Zurück">>>))
