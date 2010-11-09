;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server.documentation)

(def project :hu.dwim.web-server)

(def method make-project-tab-pages ((component project/detail/inspector) (project (eql (find-project :hu.dwim.web-server))))
  (append (call-next-method)
          (list (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Demo"))
                  (hu.dwim.web-server.test:make-component-demo-content))
                (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "User guide"))
                  (make-value-inspector (find-book 'user-guide))))))

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (chapter (:title "What is hu.dwim.web-server?")
      (paragraph ()
        "It is a scalable pure lisp-from-the-socket web server."))
    (chapter (:title "Why not something else?")
      (paragraph ()
        "There are other web servers in Common Lisp.")))
  (chapter (:title "Supported Platforms")
    (chapter (:title "Operating Systems")
      (paragraph ()
        "Linux"))
    (chapter (:title "Common Lisp Implementations")
      (paragraph ()
        "SBCL")))
  (chapter (:title "Install Guide")
    )
  (chapter (:title "Startup Server")
    )
  (chapter (:title "Shutdown Server")
    )
  (chapter (:title "Debug")
    )
  (chapter (:title "Tutorial")
    (chapter (:title "Hello world")
      ))
  (chapter (:title "Concepts")
    (chapter (:title "HTTP Server")
      (chapter (:title "Server")
        )
      (chapter (:title "Listen Entry")
        )
      (chapter (:title "Entry point")
        ))
    (chapter (:title "Application Server")
      (chapter (:title "Application")
        )
      (chapter (:title "Session")
        )
      (chapter (:title "Frame")
        )
      (chapter (:title "Request")
        )
      (chapter (:title "Response")
        )
      (chapter (:title "Action")
        )
      (chapter (:title "File Serving")
        )
      (chapter (:title "JavaScript File Serving")
        ))
    (chapter (:title "Component Server")
      (chapter (:title "Component")
        )
      (chapter (:title "Immediate")
        )
      (chapter (:title "Layout")
        )
      (chapter (:title "Widget")
        )
      (chapter (:title "Presentation")
        (chapter (:title "Maker")
          )
        (chapter (:title "Viewer")
          )
        (chapter (:title "Editor")
          )
        (chapter (:title "Inspector")
          )
        (chapter (:title "Filter")
          ))
      (chapter (:title "Text")
        (chapter (:title "Book")
          )
        (chapter (:title "Chapter")
          )
        (chapter (:title "Paragraph")
          ))
      (chapter (:title "Source")
        (chapter (:title "Variable")
          )
        (chapter (:title "Function")
          )
        (chapter (:title "Class")
          )
        (chapter (:title "Dictionary")
          )
        (chapter (:title "Package")
          )
        (chapter (:title "System")
          ))
      (chapter (:title "Chart")
        (chapter (:title "Pie")
          )
        (chapter (:title "Column")
          )
        (chapter (:title "Line")
          )
        (chapter (:title "Flow")
          ))
      (chapter (:title "Authentication")
        (chapter (:title "Login")
          )
        (chapter (:title "Logout")
          ))
      (chapter (:title "Authorization")
          ))))

(def dictionary render ()
  to-be-rendered-component? mark-to-be-rendered-component mark-rendered-component render-component render-component-layer)

(def dictionary editing ()
  editable/mixin editable-component? edited-component? begin-editing save-editing cancel-editing store-editing revert-editing join-editing leave-editing)

#|
1. WUI
------

An all-lisp web server, based on iolib.

(test-system :hu.dwim.web-server)

(start-test-server-with-wudemo-application)

http://localhost.localdomain:8080/

(setf hu.dwim.web-server::*debug-on-error* t)

2. DOJO
-------

You need to build dojo for WUI to work:

svn checkout -r 18738 http://svn.dojotoolkit.org/src/trunk/ dojo/
svn update -r 18738
$DWIM_WORKSPACE/wui/etc/build-dojo.sh --dojo $DWIM_WORKSPACE/dojo --dojo-release-dir $DWIM_WORKSPACE/wui/wwwroot/ --profile $DWIM_WORKSPACE/wui/etc/wui.profile.js --locales "en-us,hu"
|#
