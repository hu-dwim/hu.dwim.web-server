;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(log.debug "Started evaluating main.sencha.js of hu.dwim.web-server")

(Ext.namespace "hdws")
(Ext.namespace "hdws.io")
(Ext.namespace "hdws.i18n")
(Ext.namespace "hdws.field")
(Ext.namespace "hdws.help")

(defun $ (id)
  (return (Ext.getDom id)))

(defun hdws.addOnLoad (function)
  (Ext.onReady function))

(log.debug "Finished evaluating main.sencha.js of hu.dwim.web-server")
