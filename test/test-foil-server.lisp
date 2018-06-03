(defconstant ABSOLUTE_PATH "/home/chrissi/essay/cl-jana/")

(load (concatenate 'string ABSOLUTE_PATH "defsystem.cl"))

(setq mk::*load-source-if-no-binary* t)
(setq mk::*central-registry* (concatenate 'string ABSOLUTE_PATH "systemdefs/"))

(mk:operate-on-system 'portable 'load)
(mk:operate-on-system 'foil 'load)

(use-package :foil)
(setf *fvm* (make-instance 'foreign-vm
                           :stream
                           (portable-sockets::open-connection-to-host "localhost" 13579)))

(foil::def-foil-class "javax.swing.JOptionPane")
(|javax.swing|::joptionpane.showmessagedialog nil "Hello Fee!")

(foil::def-foil-class "fee.FeeServer")

(defvar *server*)
(setq *server* (|fee|::feeserver.getinstance))
; (time (|fee|::feeserver.setFullMode *server*))
; (time (|fee|::feeserver.analyze *server* "java.lang.Class"))
