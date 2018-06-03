;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        project.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;;   Support for Jana Projects
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Wed Jun  3 12:40:51 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:23:06 2010 (+0100)
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

(defclass project ()
  ((project-name
    :ACCESSOR project-name
    :INITARG :name
    :TYPE string
    :DOCUMENTATION "The name by which a project can be identified.")
   (project-directory
    :ACCESSOR project-pathname
    :INITARG :project-pathname
    :TYPE pathname
    :DOCUMENTATION "The pathname of the project-directory.")
   (project-library-directory
    :ACCESSOR project-lib-pathname
    :INITARG :project-lib-pathname
    :TYPE pathname
    :DOCUMENTATION "A relative file-name indicating the directory where the library .jar files are stored that the project depends on.")   
   (project-analysis-directory
    :ACCESSOR analysis-pathname
    :INITARG :analysis-pathname
    :TYPE pathname
    :DOCUMENTATION "The pathname of the directory where the Jana intermediate-representation of the project's .class files is stored.")
   (jar-file
    :ACCESSOR jar-file
    :INITARG :jar-file
    :TYPE string
    :DOCUMENTATION "The fully qualified pathname string of the jar file in which the class files belonging to the project are located.")
   (project-classname-dictionary-file
    :ACCESSOR classname-dictionary-pathname
    :INITARG :classname-dictionary-pathname
    :TYPE pathname
    :DOCUMENTATION "A pathname that corresponds to the file that contains the classname-dictionary.")
   (project-debug-information-file
    :ACCESSOR debug-information-pathname
    :INITARG :debug-information-pathname
    :TYPE pathname
    :DOCUMENTATION "A pathname that corresponds to the file where debug-information is exported to.")
   (library-jar-files
    :ACCESSOR lib-jar-files
    :INITARG :lib-jar-files
    :TYPE list
    :DOCUMENTATION "A classpath")
   (analysis-filename-extensions
    :ACCESSOR analysis-filename-extensions
    :TYPE list
    :DOCUMENTATION "The Filename Extensions for compressed and uncompressed files that were produced in the analysis.")
   (class-names
    :ACCESSOR class-names
    :INITARG :classes
    :TYPE list
    :DOCUMENTATION "The fully qualified class-names of classes in the project that do not carry aspect information."))
  (:DOCUMENTATION "General informations about Java projects."))

(defun read-project-record (stream)
  "Reads a project record from STREAM"
  (let ((project-record ())
	(current-package cl:*package*))
    (in-package :JANA.JAVA)
    (setq project-record
	  (read stream nil nil))
    (setq cl:*package* current-package)
    (when (debug-mode)
      (format t "~%~S" project-record))
    (unless (eql (first project-record) 'project)
      (error 'simple-error :format-control "Invalid project record. ~A" :format-arguments project-record))
    (cdr project-record)))

(defmethod repository-pathname ((self project))
  "RETURNS the pathname of the repository-directory where the project is stored."
  (io.fad:parent-directory-pathname (project-pathname self)))

(defmethod directory-name-relative-to-repository ((self project) pathname)
  "RETURNS the namestring of the relative location of PATHNAME
w.r.t the repository directory where the project is stored."
  (namestring
   (io.fad:pathname-relative-to-directory pathname (repository-pathname self))))

(defmethod uncompressed-filename-extension ((self project))
  "RETURNS a string naming the filename-extension that is used
for files that store uncompressed analysis results in the project SELF."
  (assoc-value (assoc 'uncompressed (analysis-filename-extensions self))))

(defmethod compressed-filename-extension ((self project))
  "RETURNS a string naming the filename-extension that is used
for files that store compressed analysis results in the project SELF."
  (assoc-value (assoc 'compressed (analysis-filename-extensions self))))

