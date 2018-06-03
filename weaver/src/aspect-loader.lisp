;; association list (project ((project-jar-file "C:\\myprojects\\foo.jar") (project-aspects "user.project.MyAspect" "user.project.MyOtherAspect") (project-classes "user.project.MyClass" "user.project.util.MyBStarTree")))

;; (defun establish-connection (&optional hostname port-number)
;;  "establishes a connection to the foil server. RETURNS a new connection object.")
;;

;; TODO: clean up the mess! -> move related classes into separate .lisp files
;;       add packages and system definitions

;;; crufty code ahead!

(defconstant ABSOLUTE-PATH "/home/chrissi/essay/cl-jana/")
(defconstant REPOSITORY-DIRECTORY "/home/chrissi/eclipse-workspaces/eclipse-jada/jana/test-repository/")

(load (concatenate 'string ABSOLUTE-PATH "defsystem.cl"))

(setq mk::*load-source-if-no-binary* t)
(setq mk::*central-registry* (concatenate 'string ABSOLUTE-PATH "systemdefs/"))

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
(defvar *project*)
(setq *server* (|fee|::feeserver.getinstance))
(|fee|::feeserver.setCompactMode *server*)
;; hmmph - should look up all files named "project-*.lisp" instead and analyze them one after the other
(setq *project*
      (load-project-from-file (concatenate 'string REPOSITORY-DIRECTORY "project-test-project.lisp")))
(|fee|::feeserver.setProjectJarFile *server* (jar-file *project*))
; (dolist (qualified-class-name (class-names *project*))
; (|fee|::feeserver.analyze qualified-class-name))
;; should work too -- hopefully
(|fee|::feeserver.analyze *server* (class-names *project*))
(|fee|::feeserver.analyze *server* (class-names *project*))
;; now load the classname map

;(setq jana.metamodel::*class-repository-directory* (merge-pathname REPOSITORY-DIRECTORY))


