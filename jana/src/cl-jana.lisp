;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        cl-jana.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mi Dez  9 14:24:02 2009 (+0100)
;;; 
;;; Last-Updated: Mi Jan  6 18:17:14 2010 (+0100)
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

(in-package :cl-jana)

(defvar *mostly-harmless* t)

(defun mostly-harmless (boolean-value)
  (setq *mostly-harmless* boolean-value))

(defvar *opt-spec*
  '((("project-name" #\p) :type string 
     :documentation "The name of the project that is analyzed.")
    (("repository-directory" #\r) :type string 
     :documentation "The name of the directory where the project is located.")
    (("help" #\h ) :optional t
     :documentation "Print this Usage Information.")))
;;;

(defun print-error-and-quit (err)
  (terpri)
  (format t "~%~80{-~}" '(nil))
  (format t "~%ERROR: ~A" err) 
  (format t "~%~80{-~}" '(nil))
  (terpri)
  (bye))

(defmacro quit-on-error (&body forms)
   `(handler-bind ((error #'print-error-and-quit))
      ,@forms))

(defun  bye ()
  #+LISPWORKS (lispworks:quit)
  #+CCL (ccl:quit)
  #+SBCL (sb-ext:quit)
  #+CLISP (ext:quit))

(defun valid-options-p (&key project-name repository-directory runtime-monitor no-static-analyses help)
  "RETURNS T if  project-name and repository-directory are valid strings."
  (declare (ignore runtime-monitor no-static-analyses))
  (and 
   (not help)
   (typep project-name 'string)
   (typep repository-directory 'string)
   (> (length project-name) 0)
   (> (length repository-directory) 0)))

(defun jana (&key project-name repository-directory (runtime-monitor "") (no-static-analyses nil))
  "Starts the Transformation that adds the run-time monitor denoted by the 
fully-qualified class-name string RUNTIME-MONITOR to the project named PROJECT-NAME,
that can be found in the filesystem under the path-name string REPOSITORY-DIRECTORY.
When the generalized boolean STATIC-ANALYSES is T, static analyses are used to decide when
calls to the run-time monitor are not necessary."
  (format t "~%Transforming project '~A' from repository '~A'." project-name repository-directory)
  (when (> (length runtime-monitor) 0)
    (format t "~%Runtime monitor used: ~A" runtime-monitor))
  (unless no-static-analyses
    (format t "~%Applying Static Analyses"))
  (add-synchronization-analysis project-name repository-directory runtime-monitor no-static-analyses))

(defun main ()
  (quit-on-error
    (disable-multiprocessing)
    (unless *mostly-harmless*
      (enable-multiprocessing)
      (setq *opt-spec*
            (nconc *opt-spec*
                   '((("runtime-monitor" #\m) :type string :optional t 
                      :documentation "A class implementing the run-time monitor interface.")
                     (("no-static-analyses" #\a) :optional t 
                      :documentation "Disables static anlayses.")))))
    (multiple-value-bind (options arguments)
        (command-line-arguments:process-command-line-options *opt-spec* (command-line-arguments:get-command-line-arguments))
      (when (debug-mode)
        (format t "~%Options: ~A" options)
        (format t "~%Arguments: ~A" arguments))
      (cond ((apply #'valid-options-p options)
             (apply #'jana options))
            (t
             (format t "~%Usage:")
             (command-line-arguments:show-option-help *opt-spec*))))
    (bye)))
