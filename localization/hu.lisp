;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; Util

(def localization hu
  (sequence.empty "üres")
  (sequence.element "elem"))

;;;;;;
;;; Mime type

(def localization hu
  (mime-type.application/msword "Microsoft Word Dokumentum")
  (mime-type.application/vnd.ms-excel "Microsoft Excel Dokumentum")
  (mime-type.application/pdf "PDF Dokumentum")
  (mime-type.image/png "PNG kép")
  (mime-type.image/tiff "TIFF kép"))

;;;;;;
;;; Error handling

(def localization hu
  (error.internal-server-error "Ismeretlen eredetű hiba")
  (error.access-denied-error "Hozzáférés megtagadva")

  (render-error-page/internal-error (&key administrator-email-address &allow-other-keys)
    <div
      <h1 "Programhiba">
      <p "A szerverhez érkezett kérés feldolgozása közben váratlan hiba történt. Elnézést kérünk az esetleges kellemetlenségért!">
      <p "A hibáról értesítést kaptak a fejlesztők, és valószínűleg a közeljövőben javítják azt.">
      ,(when administrator-email-address
         <p "Amennyiben kapcsolatba szeretne lépni az üzemeltetőkkel, azt a "
            <a (:href ,(mailto-href administrator-email-address)) ,administrator-email-address>
            " email címen megteheti.">)
      <p <a (:href "#" :onClick `js-inline(history.go -1)) "Vissza">>>)

  (render-error-page/access-denied (&key &allow-other-keys)
    <div
      <h1 "Hozzáférés megtagadva">
      <p "Nincs joga a kívánt oldal megtekintéséhez.">
      <p <a (:href "#" :onClick `js-inline(history.go -1)) "Vissza">>>))
