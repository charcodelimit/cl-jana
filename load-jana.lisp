(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (in-package :cl-user)

  (defvar *make-release* nil)

  (defparameter *cl-jana-base-directory*
    (make-pathname :name nil :type nil :version nil
                   :defaults (parse-namestring *load-truename*)))

  (let ((cl:*default-pathname-defaults* *cl-jana-base-directory*))

    (format t "~%Using Directory: ~A" (namestring cl:*default-pathname-defaults*))
    
    #+CCL (require :defsystem)
    #-CCL (load "defsystem.cl")
    
    (eval (read-from-string "(setq mk::*load-source-if-no-binary* t)"))
    
    (eval (read-from-string "(setq mk::*central-registry* \"systemdefs/\")"))

    #-SBCL
    (eval (read-from-string "(progn (mk:operate-on-system \"jana\" 'compile)
           (mk:operate-on-system \"tools\" 'compile))"))
    #+SBCL
    (eval (read-from-string "(progn(mk:operate-on-system \"tools\" 'load :verbose t)
          (mk:operate-on-system \"jana\" 'load :verbose t)
          (mk:operate-on-system \"fee\" 'load :verbose t))")))

  (unless *make-release*
    (setq cl:*default-pathname-defaults* *cl-jana-base-directory*)))
