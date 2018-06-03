;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-loader-implementation-dependent.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;    This is obsolete until the Java -- Common Lisp Bridge is needed again,
;;;    to analyze classes on the fly.
;;;    The current workflow is to analyze ALL  .class files first and then
;;;    load the information on demand from within cl-jana.
;;;    In this process, only those classes reachable from a project are
;;;    analyzed. The referenced classes from the Java class-libraries can be 
;;;    analyzed once in advance, which further increases performance.
;;;
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Jun 15 11:31:45 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:30:20 2010 (+0100)
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

(in-package :JANA.MX.JAVA)

;; Java - FEE Specific 

(defvar *class-repository-directory* "test-repository")
(defvar +java-foil-classpath+ "")

;(unless (OR (find-symbol "jana.mx.java::*class-repository-directory*")
;	    (not jana.mx.java::*class-repository-directory*))
;  (error "Jana class-file repository directory is undefined!
;Please Eval: (setq jana.mx.java::*class-repository-directory* (merge-pathname \"Insert Directory Name\"))"))

;; Foil specific constants

(defconstant +foil-port+ 13579)

;; External Java process specific constants

(defconstant +run-program-function+
  #+SBCL #'sb-ext:run-program
  #+CMU #'extensions:run-program
  #+CLISP #'ext:run-program
  #+LISPWORKS #'system:run-shell-command
  #- (OR SBCL CMU CLISP LISPWORKS) #'identity)

(defconstant +java-command+
  #+UNIX "/usr/bin/java"
  #-UNIX "")

;; syntactic analysis frontend command line interface
(defconstant +jana-fee-cli-class+ "Jana") 

;; syntactic analysis frontend public Java interface
(defconstant +jana-fee-foil-class+ "jana.JanaAnalyzer")

;; chr: maybe add +jana-top-level-dir+
(defvar *java-analysis-classpath*
  #+UNIX "repository/java-src"
  #-UNIX "")

(defvar *java-classpath*
  (concatenate 'string
	       +java-foil-classpath+
	       ":"
	       *java-analysis-classpath*))

(defvar *jana-instance* nil)

;;; Running External Java Process
(defun run-java (arguments)
#+CCL (declare (ignore arguments))
#-CCL
  (with-output-to-string (stream)                         
    #+CLISP 
    (progn
      (let ((is (funcall +run-program-function+
                         +java-command+
                         :arguments arguments
                         :output :stream)))
        (do ((c (read-char is) (read-char is nil 'the-end)))
            ((not (characterp c))) (write-char c stream))))
    #-CLISP
    (funcall +run-program-function+
	     +java-command+
	     arguments :output stream)))
;; chr: ToDo!    #+LISPWORKS (funcall +run-program-function+ (list +java-command arguments) )

;;; Remote Connection to Java through Foil

(defun connect-to-jvm ()
  (setq foil:*fvm*
        (funcall `(make-instance 'foil:foreign-vm
                   :stream (portable-sockets:open-connection "localhost"
                            +foil-port+))))
  (setq *jana-instance* 
        (funcall `(foil:def-foil-class "jana.JanaAnalyzer")))
  (funcall (read-from-string "|jana|::setrepositorydirectory") *class-repository-directory*))

;;; chr: add error handling!
(defun analyze-using-foil (class-name)
  "Analyzes the class with the given CLASS-NAME.
A connection to a running Foil Server is established if necessary.
RETURNS t if the call was successful."
  (unless foil:*fvm*
    (connect-to-jvm))
  (when *jana-instance*
    (funcall (read-from-string "|jana|::analyzeclass") class-name)))

;;; chr: test!
(defun analyze-using-system-process (class-name)
  (run-java (list "-cp " *java-classpath* +jana-fee-cli-class+ "-directory " *class-repository-directory* class-name)))
