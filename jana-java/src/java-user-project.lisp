;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: ; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-user-project.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fr Nov 20 15:48:23 2009 (+0100)
;;; 
;;; Last-Updated: Mi Jan  6 18:23:11 2010 (+0100)
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

(defclass java-user-project (java-project)
  ((project-compilation-directory
    :ACCESSOR compilation-pathname
    :INITARG :compilation-pathname
    :TYPE pathname
    :DOCUMENTATION "The pathname of the directory where the .class files are stored after compiling the transformation results.")
   (project-transformation-directory
    :ACCESSOR transformation-pathname
    :INITARG :transformation-pathname
    :TYPE pathname
    :DOCUMENTATION "The pathname of the directory where the transformation results in JIMPLE intermediate representation are stored.")     
   (final-jar-file
    :ACCESSOR final-jar-file
    :INITARG :final-jar-file
    :TYPE string
    :DOCUMENTATION "The fully qualified pathname string of the jar file in which the results of compilation are stored.")
   (aspect-names
    :ACCESSOR aspect-names
    :INITARG :aspects
    :TYPE list
    :DOCUMENTATION "The fully qualified class-names of classes that contain aspect-information.")
   (transformed-class-names
    :ACCESSOR transformed-class-names
    :INITARG :transformed-classes
    :TYPE list
    :DOCUMENTATION "The fully qualified class-names of classes in the project after transformation.")
   (jana-aspects
    :ACCESSOR aspects
    :INITFORM (make-hash-table :test #'equal)
    :TYPE hash-table
    :DOCUMENTATION "A Hashtable mapping class-names of the aspects in a project to their corresponding jana metamodel instances.")
   (java-runtime-libraries
    :ACCESSOR java-runtime-libraries
    :TYPE java-project
    :DOCUMENTATION "The java-project containing the Java-Runtime libraries metamodel."))
  (:DOCUMENTATION "User projects can contain aspects, and sourcecode in these projects can be transformed."))

(defmethod initialize-java-project ((self java-user-project))
  "Initializes the  java-runtime-libraries project that is used  
by the java-user-project instance SELF to load metamodel 
instances of system classes."
  (call-next-method)
  (setf (java-runtime-libraries self)
          (load-java-rt-project   (repository-pathname self))))

(defun load-java-user-project (project-name repository-directory-pathname)
  "Loads the java-user-project named PROJECT-NAME from the directory located
at REPOSITORY-DIRECTORY-PATHNAME.
RETURNS a new fully initialized java-user-project instance."
  (load-java-project-from-file 
   (project-filename-for-name project-name)
   repository-directory-pathname 
   'deserialize-user-project))

(defmethod serialize-project-data ((self java-user-project))
  "Serializes the information of the java-user-project SELF that should 
be written to the project-file into an association list."
  (let ((alist '()))
    (declare (type list alist))
    (push (cons 'project-compilation-directory
                (directory-name-relative-to-repository self (compilation-pathname self)))
          alist)
    (push (cons 'project-transformation-directory
                (directory-name-relative-to-repository self (transformation-pathname self)))
          alist)
    (push (cons 'project-final-jar-file
                (final-jar-file self))
          alist)
    (push (cons 'project-aspects
                (aspect-names self))
          alist)
    (push (cons 'project-transformed-classes
                (transformed-class-names self))
          alist)
    (nconc (call-next-method self) (nreverse alist))))

(defun deserialize-user-project (alist repository-directory-pathname)
  "RETURNS a new unitialized java-user-project instance 
using the data from the association list ALIST."
  (let ((instance nil)
        ;; chr: bad cruft -- use setf slot-value instead and remove the initargs!
        (project-name (assoc-value (assoc 'project-name alist)))
        (java-version (assoc-value (assoc 'java-version alist)))
        (project-directory-filename (assoc-value (assoc 'project-directory alist)))
        (library-directory-filename (assoc-value (assoc 'project-library-directory alist)))
        (analysis-directory-filename (assoc-value (assoc 'project-analysis-directory alist)))
        (compilation-directory-filename (assoc-value (assoc 'project-compilation-directory alist)))          
        (transformation-directory-filename (assoc-value (assoc 'project-transformation-directory alist)))
        (project-jar-file (assoc-value (assoc 'project-jar-file alist)))
        (project-final-jar-file (assoc-value (assoc 'project-final-jar-file alist)))          
        (classname-dictionary-filename (assoc-value (assoc 'project-classname-dictionary-file alist)))
        (debug-information-filename (assoc-value (assoc 'project-debug-information-file alist)))
        (project-library-jar-files (cdr (assoc 'project-library-jar-files alist)))
        (project-aspects (cdr (assoc 'project-aspects alist)))
        (project-classes (cdr (assoc 'project-classes alist)))
        (project-transformed-classes (cdr (assoc 'project-transformed-classes alist))))
    (unless project-jar-file
      (setq project-jar-file ""))
    (setq instance
          (make-instance 'java-user-project
                         :name project-name
                         :project-pathname (io.fad:relative-pathname-as-directory repository-directory-pathname project-directory-filename)
                         :project-lib-pathname (io.fad:relative-pathname-as-directory repository-directory-pathname library-directory-filename)
                         :analysis-pathname (io.fad:relative-pathname-as-directory repository-directory-pathname analysis-directory-filename)
                         :compilation-pathname (io.fad:relative-pathname-as-directory repository-directory-pathname compilation-directory-filename)                           
                         :transformation-pathname (io.fad:relative-pathname-as-directory repository-directory-pathname transformation-directory-filename)
                         :jar-file project-jar-file
                         :final-jar-file project-final-jar-file                    
                         :classname-dictionary-pathname (io.fad:relative-pathname-as-file repository-directory-pathname classname-dictionary-filename)
                         :debug-information-pathname (io.fad:relative-pathname-as-file repository-directory-pathname debug-information-filename)
                         :lib-jar-files project-library-jar-files       
                         :aspects project-aspects
                         :classes project-classes
                         :transformed-classes project-transformed-classes))
    (setf (analysis-filename-extensions instance) 
          (assoc-value (assoc 'project-analysis-filename-extensions alist)))
    (setf (java-version instance) java-version)
    instance))

(defmethod java-class ((self java-user-project) fully-qualified-classname)
  "RETURNS the jana-class that has the classname FULLY-QUALIFIED-CLASSNAME.
If no such class has been loaded yet, it is loaded from the filesystem."
  (declare #.*standard-optimize-settings*)
  (cond ((translate-class-name self fully-qualified-classname)
         (call-next-method))
        (t
         (java-class (java-runtime-libraries self) fully-qualified-classname))))