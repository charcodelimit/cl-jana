(load "portable-threads.lisp")
(load "portable-sockets.lisp")
(load "foil.lisp")

(use-package :foil)
(setf *fvm* (make-instance 'foreign-vm
                           :stream
                           (portable-sockets::open-connection-to-host "localhost" 13579)))

(foil::def-foil-class "javax.swing.JOptionPane")
(|javax.swing|::joptionpane.showmessagedialog nil "Hello Foo!")
; (foil::def-foil-class "java.lang.System")
; (|java.lang|::system.currenttimemillis)

