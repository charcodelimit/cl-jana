;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-user; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        make-internal-release.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;  Create an executable with multi-threaded analyses enabled
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Do Dez 10 15:34:45 2009 (+0100)
;;; 
;;; Last-Updated: Mi Jan  6 22:39:12 2010 (+0100)
;;;           By: Christian Hofmann
;;; 
;;; Copyright (C) 2009, Christian Hofmann. All rights reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;; 
;;;****************************************************************************

(in-package :cl-user)
(defvar *make-release* t)

(defpackage :jana-packager
  (:use :cl))
(in-package :jana-packager)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *application-name* "cl-jana-internal")
  (defvar *system-loader* "load-jana")
  (defvar *init-function* "#'jana:main"))

(defun  bye ()
  #+LISPWORKS (lispworks:quit)
  #+CCL (ccl:quit)
  #+SBCL (sb-ext:quit)
  #+CLISP (ext:quit))

(defun make-release ()
  (let ((start 0)
        (end 0)
        (executable-name *application-name*)
        (date (multiple-value-list (get-decoded-time))))
    (setq start (get-internal-real-time))
    ;; common-lisp implementation
    (cond
      ((member :ccl *features*)
       (setq executable-name (concatenate 'string executable-name "-ccl")))
      ((member :clisp *features*)
       (setq executable-name (concatenate 'string executable-name "-clisp")))
      ((member :sbcl *features*)
       (setq executable-name (concatenate 'string executable-name "-sbcl")))
      ((member :lispworks *features*)
       (setq executable-name (concatenate 'string executable-name "-lispworks"))))
    ;; os
    (cond
      ((member :linux *features*)
       (setq executable-name (concatenate 'string executable-name "-linux")))
      ((or (member :win32 *features*)
           (member :win64 *features*)
           (member :windows *features*))
       (setq executable-name (concatenate 'string executable-name "-windows")))
      ((or (member :osx *features*)
           (member :darwin *features*))
       (setq executable-name (concatenate 'string executable-name "-osx")))
      ((member :unix *features*)
       (setq executable-name (concatenate 'string executable-name "-unix"))))
    ;; architecture
    (cond
      ((or (member :ppc *features*)
           (member :powerpc *features*))
       (setq executable-name (concatenate 'string executable-name "-ppc")))
      ((or (member :x86-64 *features*)
           (and (member :word-size=64 *features*)
                (member :pc386 *features*)))
       (setq executable-name (concatenate 'string executable-name "-x86_64")))
      ((or (member :x86 *features*)
           (and (member :word-size=32 *features*)
                (member :pc386 *features*)))
       (setq executable-name (concatenate 'string executable-name "-x86"))))
    ;; date
    (setq executable-name (concatenate 'string executable-name "-" (princ-to-string (nth 3 date))))
    (setq executable-name (concatenate 'string executable-name "_" (princ-to-string (nth 4 date))))
    (setq executable-name (concatenate 'string executable-name "_" (princ-to-string (nth 5 date))))
    ;; executable file-type
    (cond ((member :unix *features*)
           (setq executable-name (concatenate 'string executable-name ".bin")))
          ((or (member :win32 *features*)
	       (member :win64 *features*)
	       (member :windows *features*))
           (setq executable-name (concatenate 'string executable-name ".exe")))
          ((or (member :osx *features*)
               (member :darwin *features*)
               (member :macos *features*))
           ;; chr: is that right ?
           (setq executable-name (concatenate 'string executable-name ".app"))))
    (prepare-image *system-loader*)
    (progn
      (setq end (get-internal-real-time))
      (format t "~%~80{-~}" '(nil))
      (format  t "~%Loading & Compiling: ~fs" (/ (- end start) cl:internal-time-units-per-second)))
    (save-image executable-name (eval (read-from-string *init-function*)))
    (progn
      (setq end (get-internal-real-time))
      (format t "~%~80{-~}" '(nil))
      (format  t "~%~fs" (/ (- end start) cl:internal-time-units-per-second)))
    (bye)))

(defun prepare-image (file-name)
  "Compiles and Loads the System"
    (load file-name)
    (funcall (eval (read-from-string "#'cl-jana::mostly-harmless")) nil)
    (room))

(defun save-image (executable-name init-function)
  "Saves the Image"
  #+CCL (ccl:save-application executable-name :toplevel-function init-function :error-handler :quit :prepend-kernel t)
  #+SBCL (sb-ext:save-lisp-and-die executable-name :toplevel init-function :executable t :save-runtime-options t)
  #+CLISP (ext:saveinitmem executable-name :quiet t :init-function init-function :norc t :script t :executable t))

(eval-when (:load-toplevel :execute)
  (make-release))
