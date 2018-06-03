;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple.conversion; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-project.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;
;;;    This file contains the conversion of projects to a set of
;;;    .jimple files. These can be used to create Java .class files
;;;    with the corresponding compiler back-end.
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fri Dec 19 21:38:33 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:25:41 2010 (+0100)
;;;           By: Christian Hofmann
;;; 
;;; Copyright (C) 2008-2009, Christian Hofmann. All rights reserved.
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

(in-package :JANA.JAVA.JIMPLE.CONVERSION)

(defgeneric save-java-project-classes (java-project output-directory)
  (:DOCUMENTATION "Stores the classes and aspects defined in JAVA-PROJECT in .jimple files
in the directory OUTPUT-DIRECTORY."))

(defmethod write-jimple-statement-to-file ((self java-classifier-declaration)
					   (repository-directory pathname))
  "Writes the Jimple-IL representation of a java-classifier-declaration SELF
to a .jimple file in the directory denoted by the pathname REPOSITORY-DIRECTORY.
The naming convention for .jimple files is that the name consists of the 
fully-qualified-classname and the '.jimple' filetype designator.
Classes in files deviating from this naming scheme cannot be found 
during compilation, as the file-name establishes the mapping between class-names
and files."
  (declare #.*standard-optimize-settings*)
  (let ((jimple-file))
    (setq jimple-file
	  (merge-pathnames
	   (make-pathname :name (qualified-name self)
			  :type +JIMPLE-FILENAME-EXTENSION+)
	   repository-directory))
    (with-open-file (output-stream jimple-file :direction :output :if-exists :supersede)
      (write-jimple-statement self output-stream))))

(defmethod write-class-table-to-directory ((keys list)
                                           (table hash-table)
					   (directory pathname))
  "Writes the Jimple-IL representation of classes stored in a hash-table
TABLE to a .jimple file in the directory denoted by the pathname
REPOSITORY-DIRECTORY."
  (declare #.*standard-optimize-settings*)
  (loop
      :for class-name :in keys
      :when (gethash class-name table)
        :do
        ;;(format t "~%~A ~A" class-name (gethash class-name table))
        (write-jimple-statement-to-file (gethash class-name table) directory)))

(defmethod save-java-project-classes ((self java-project)
                                      (output-directory pathname))
  "Stores the classes and aspects defined in JAVA-PROJECT in .jimple files
in the directory OUTPUT-DIRECTORY."
  (declare #.*standard-optimize-settings*)
  (write-class-table-to-directory (class-names self) (classes self) output-directory)
  (write-class-table-to-directory (aspect-names self) (aspects self) output-directory))

(defmethod save-java-project ((self java-project))
  "Store the classes and aspects defined in JAVA-PROJECT in the
transformation output directory."
  (declare #.*standard-optimize-settings*)  
  (save-java-project-classes self (transformation-pathname self))
  (jana.java:update-transformed-classes self)
  (jana.java:save-java-project-file self)
  (jana.java:save-debug-info-file self))

(defmethod write-debug-information ((self java-project) output-stream)
  "Writes the debug-information for the project SELF
to the stream OUTPUT-STREAM."
  (declare #.*standard-optimize-settings*
           (type stream output-stream))
  (pprint `(("source-positions" . ,(source-positions self))
            ("source-files" . ,(source-files self)))
          output-stream))

(defmethod source-files ((self java-project))
  "Creates an association list mapping class-names to source-file-names"
  (declare #.*standard-optimize-settings*)
  (let ((class)
        (source-files '()))
    (declare (type list source-files))
    (loop :for class-name :in (class-names self)
          :when (gethash class-name (classes self))
            :do
             (setq class
                   (gethash class-name (classes self)))
             (push (list (qualified-name class)
                         (source-file-name class))
                   source-files))
    (nreverse source-files)))

#.(declaim (inline source-positions-for-method))
(defun source-positions-for-method (method-implementation)
  "Creates a list with the line-numbers of each instruction 
in the java-method-implementation METHOD-IMPLEMENTATION.
RETURNS a list with either a pair of  the form (method-name . line-number-list)
or two pairs where the first one uses the quoted method name,
 and the second one the original method-name."
  (declare #.*standard-optimize-settings*)
  (let ((source-positions '())
        (line-numbers '())
        (reversed-line-numbers '()))
    (declare (type list source-positions line-numbers reversed-line-numbers))
    ;; collect line-numbers
    (dolist (instruction (instructions (body method-implementation)))
      (if (slot-boundp instruction 'line-number)
          (push (line-number instruction)
                line-numbers)
          (push -1 line-numbers)))
    (setq reversed-line-numbers 
          (nreverse line-numbers))
    ;; add line-numer table for methods with quoted names twice:
    ;; ... with quoting,
    (when (gethash (qualified-name method-implementation) *jimple-token-table*)
      (push (list (with-output-to-string (out) (write-java-method-signature method-implementation out :quote-method-name t))
                  reversed-line-numbers)
            source-positions))
    ;; ... and without quoting
    (push (list (with-output-to-string (out) (write-java-method-signature method-implementation out))
                reversed-line-numbers)
          source-positions)
    (nreverse source-positions)))

(defmethod source-positions ((self java-project))
  "Creates a nested association list that associates lists of
line-numbers with methods and classes.
RETURNS a list of the form:
((qualified-class-name ((method-signature line-number-list) ...)) ...)."
  (declare #.*standard-optimize-settings*)
  (let ((source-positions '())
        (method-list '()))
    (declare (type list source-positions method-list))
    (loop :for class-name :in (transformed-class-names self)
          :when (gethash class-name (classes self))
          :do
             (setq method-list (list))
             (dolist (method (methods (gethash class-name (classes self))))
                    (when (typep method 'java-method-implementation)
                      (dolist (method-source-positions (source-positions-for-method method))
                        (push method-source-positions method-list))))
                (push (list class-name
                            (nreverse method-list))
                   source-positions))
    (nreverse source-positions)))
