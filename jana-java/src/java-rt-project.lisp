;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: ; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-rt-project.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;    This file contains the definition of the java run-time library project.
;;;    This project is used to manage information about the 
;;;     Java classes in the Java Runtime Libraries.
;;;
;;; COMMENTS
;;;   chr: it might be necessary that this project contains informations
;;;           about all classes in the boot classpath
;;; 
;;; Author: Christian Hofmann (chr)
;;; 
;;; Created: Fr Nov 20 13:48:07 2009 (+0100)
;;; 
;;; Last-Updated: Mi Jan  6 18:23:14 2010 (+0100)
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

(in-package :JANA.JAVA)

(defconstant +java-rt-project-name+ "java")

(defclass java-rt-project (java-project)
  ()
  (:DOCUMENTATION "Manages the jana metamodel instances that represent the classes in the Java RunTime libraries."))

#.(declaim (inline load-java-rt-project))
(defun load-java-rt-project (repository-directory-pathname)
  "Loads the java-rt-project from the directory located
at REPOSITORY-DIRECTORY-PATHNAME."
  (load-java-project-from-file 
   (project-filename-for-name +java-rt-project-name+) 
   repository-directory-pathname 
   'deserialize-rt-project))

(defun deserialize-rt-project (alist repository-directory-pathname)
  "RETURNS a new unitialized java-rt-project instance 
using the data from the association list ALIST."
  (let ((instance nil)
        ;; chr: bad cruft -- use setf slot-value instead and remove the initargs!
        (project-name (assoc-value (assoc 'project-name alist)))
        (project-jar-file (assoc-value (assoc 'project-jar-file alist)))
        (project-pathname (assoc-value (assoc 'project-directory alist)))
        (project-library-pathname (assoc-value (assoc 'project-library-directory alist)))
        (project-analysis-pathname (assoc-value (assoc 'project-analysis-directory alist)))
        (classname-dictionary-pathname (assoc-value (assoc 'project-classname-dictionary-file alist)))
        (debug-information-pathname (assoc-value (assoc 'project-debug-information-file alist))))
    (unless project-jar-file
      (setq project-jar-file ""))
    (setq instance
          (make-instance 'java-rt-project
                         :name project-name))
    (setf (java-version instance)
          (assoc-value (assoc 'java-version alist)))
    (setf (project-pathname instance)
          (io.fad:relative-pathname-as-directory  repository-directory-pathname project-pathname))
    (setf (project-lib-pathname instance)
          (io.fad:relative-pathname-as-directory repository-directory-pathname project-library-pathname))
    (setf (analysis-pathname instance)
          (io.fad:relative-pathname-as-directory repository-directory-pathname project-analysis-pathname))
    (setf (classname-dictionary-pathname instance)
          (io.fad:relative-pathname-as-file repository-directory-pathname classname-dictionary-pathname))
    (setf (debug-information-pathname instance)
          (io.fad:relative-pathname-as-file repository-directory-pathname debug-information-pathname))
    (setf (jar-file instance)  project-jar-file)
    (setf (lib-jar-files instance)
          (cdr (assoc 'project-library-jar-files alist)))
    (setf (analysis-filename-extensions instance)
          (assoc-value (assoc 'project-analysis-filename-extensions alist)))
    (setf (class-names instance)
          (cdr (assoc 'project-classes alist)))
    instance))