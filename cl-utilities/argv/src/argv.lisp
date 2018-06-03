;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license. See LICENSE  ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2009 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Francois-Rene Rideau                            ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+xcvb (module (:depends-on ("pkgdcl")))
;; let's not DEPEND on (:build "/cl-launch") since
;; that opens a can of worm for people using ASDF.

(in-package :command-line-arguments)

(defun get-command-line-arguments ()
  (if (find-package :cl-launch)
    (symbol-value (find-symbol (string :*arguments*) :cl-launch))
    (progn
      #+sbcl (cdr sb-ext:*posix-argv*)
      #+clozure (cdr (ccl::command-line-arguments))
      #+gcl (cdr si:*command-args*)
      #+ecl (loop for i from 1 below (si:argc) collect (si:argv i))
      #+cmu (cdr extensions:*command-line-strings*)
      #+allegro (cdr (sys:command-line-arguments))
      #+lispworks (cdr sys:*line-arguments-list*)
      #+clisp ext:*args*
      #-(or sbcl clozure gcl ecl cmu allegro lispworks clisp)
      (error "get-command-line-arguments not supported for your implementation"))))

(defun compute-and-process-command-line-options (specification)
  (process-command-line-options specification (get-command-line-arguments)))

(defun invoke-command-line-handler (function options arguments &key
                                    (positional-arity 0) (rest-arity nil) name)
  (let ((l (length arguments)))
    (unless (>= l positional-arity)
      (error "~@[~A: ~] Too few arguments. Expected~@[ at least~] ~A, got ~A ~S"
             name rest-arity positional-arity l arguments))
    (when (and (> l positional-arity) (not rest-arity))
      (error "~@[~A: ~] Too many arguments. Expected only ~A, got ~A ~S"
             name positional-arity l arguments))
    (let ((positional-arguments (subseq arguments 0 positional-arity))
          (rest-arguments (when rest-arity (subseq arguments positional-arity))))
      (apply function (append positional-arguments
                              (etypecase rest-arity
                                (null nil)
                                ((eql t) (list rest-arguments))
                                (keyword (list rest-arity rest-arguments)))
                              options)))))

(defun handle-command-line (specification function
                            &key (positional-arity 0) (rest-arity nil) name
                            (command-line (get-command-line-arguments)))
  (multiple-value-bind (options arguments)
      (process-command-line-options specification command-line)
    (invoke-command-line-handler function options arguments
                                 :name name
                                 :positional-arity positional-arity
                                 :rest-arity rest-arity)))
