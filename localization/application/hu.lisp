;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def localization hu
  (render-application-internal-error-page (&key administrator-email-address &allow-other-keys)
    <div
     <p "A hibáról értesülni fognak a fejlesztők és remélhetőleg a közeljövőben javítják azt.">
     ,(when administrator-email-address
        <p "Amennyiben kapcsolatba szeretne lépni az üzemeltetőkkel, azt a "
           <a (:href ,(mailto-href administrator-email-address)) ,administrator-email-address>
           " email címen megteheti.">)>)
  )

(def js-localization hu
  (error.ajax.request-to-invalid-session "Megszakadt a kapcsolat a szerverrel, mert a várakozási idő lejárt. Az oldal újratöltésével újból felépítheti a kapcsolatot.")
  (error.ajax.request-to-invalid-frame "TODO error.ajax.request-to-invalid-frame")
  (error.network-error.title "Kommunikációs hiba")
  (error.network-error "Hiba történt a szerverrel való kommunikáció közben. Ezt okozhatja átmeneti hálózati hiba is, ezért kérem próbálja meg újratölteni az oldalt! Amennyiben a hiba továbbra is fennáll, akkor kérem lépjen kapcsolatba az üzemeltetőkkel!")
  (error.generic-javascript-error.title "Váratlan kliens oldali hiba")
  (error.generic-javascript-error "Hiba történt a böngészőben a program futtatása közben. Ezt okozhatja a böngésző hibás működése is, ezért kérem használjon friss és támogatott böngészőt! Amennyiben a hiba továbbra is fennáll egy támogatott böngészővel, akkor kérem lépjen kapcsolatba az üzemeltetőkkel!")
  (error.internal-server-error.title "Programhiba")
  (error.internal-server-error "Programhiba lépet fel, elnézést kérünk az esetleges kellemetlenségért!")
  (action.reload-page "Az oldal újratöltése")
  (action.cancel "Mégse")
  )
