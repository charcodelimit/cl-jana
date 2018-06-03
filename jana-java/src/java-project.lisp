;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;;FILE:               java-project.lisp
;;;LANGUAGE:           Common-Lisp
;;;
;;; DESCRIPTION
;;;
;;;    This file contains the definitions of projects, which are
;;;    used to manage information about the location of a project JAR-file
;;;    in the file-system and the Java classes and Java classes with Aspect
;;;    declarations that the JAR file contains.
;;;
;;; Bugs: - using (java-global-dictionary self) to load several java classes won't work
;;;         current workaround use for each load a new global-dictionary (self is a project)
;;;       - loading classes using new instances of the same project won't produce new metamodel
;;;         instances of those classes, instead a reference to the already created metamodel instance is returned
;;; 
;;; Author: Christian Hofmann
;;;
;;; Created: Fri Dec  5 00:43:51 2008 (z)
;;;
;;; Last-Updated: Mi Jan  6 18:23:15 2010 (+0100)
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
;;;****************************************************************************


(in-package :JANA.JAVA)

(defgeneric java-class (project fully-qualified-classname)
  (:DOCUMENTATION "RETURNS a java-classifier-declaration that has the fully-qualified-name
                   FULLY-QUALIFIED-CLASSNAME, which is contained in the java-project PROJECT.
                   The class must not necessarily belong to the analyzed program,
                   but can also be classes referenced by the program."))

(defgeneric load-java-class (project fully-qualified-name &key error-type)
  (:DOCUMENTATION "Loads the class denoted by the string FULLY-QUALIFIED-NAME from the project PROJECT.
If the class denoted by fully-qualified-name cannot be loaded, an error of type ERROR-TYPE is signaled."))

(defgeneric load-java-class-silently (project fully-qualified-name &key error-type)
  (:DOCUMENTATION "Loads the class denoted by the string FULLY-QUALIFIED-NAME from the project PROJECT.
If the class denoted by fully-qualified-name cannot be loaded, an error of type ERROR-TYPE is signaled.
This method outputs no progress information."))

(defgeneric load-java-classes (java-project &key error-type)
  (:DOCUMENTATION "Loads all classes belonging to the project JAVA-PROJECT.
Project external classes can be loaded on demand using java-class!
If no classes can be found, an error of type ERROR-TYPE is signaled."))

(defgeneric save-java-project-classes (self output-directory)
  (:DOCUMENTATION "Stores the classes  defined in the java-project SELF
in .jimple files in the directory OUTPUT-DIRECTORY."))

(defgeneric save-java-project (self)
  (:DOCUMENTATION  "Store the classes defined in the java-project SELF
in the transformation output directory."))

(defgeneric write-debug-information (self output-stream)
  (:DOCUMENTATION "Writes the debug-information for the project SELF
to the stream OUTPUT-STREAM."))

;; METAMODEL-instances should be created on a demand base
(defclass java-project (project)
  ((java-version
    :ACCESSOR java-version
    :INITFORM ""
    :TYPE string
    :DOCUMENTATION "A String indicating the Java Version used to create the project. This information includes the Version String and the Vendor name. ")
   (jana-classes
    :ACCESSOR classes
    :INITFORM (make-hash-table :test #'equal)
    :TYPE hash-table
    :DOCUMENTATION "A Hashtable mapping class-names  of the classes in a project to their corresponding jana metamodel instances.")
   (classname-dictionary
    :ACCESSOR classname-dictionary
    :TYPE list
    :DOCUMENTATION "The classname-dictionary is used to translate between
fully qualified Java classnames and file-names.
The file-names are used to load files containing the metamodel representation
of the class.")
   (global-scope
    :ACCESSOR global-scope
    :TYPE java-global-scope
    :DOCUMENTATION "The global-scope for the classes defined in the project.")
   (project-context
    :ACCESSOR project-context
    :TYPE java-project-context
    :DOCUMENTATION "The compile-time meta-level-context associated with the project.")
   (memoization-cache
    :ACCESSOR memoization-cache
    :TYPE java-memoization-table
    :DOCUMENTATION "The memoization-table used to create instances of jana-metamodel elements associated with the project."))
  (:DOCUMENTATION "A java-project can be asked for the jana metamodel instances that represent the aspects and classes of a project."))

(defun project-filename-for-name (project-name)
  "RETURNS the project-filename for the project named PROJECT-NAME."
  (concatenate 'string +project-suffix+ project-name +source-extension+))

(defun load-java-project (project-name repository-directory-pathname)
  "Translates the name PROJECT-NAME into the name of the corresponding
project-file and loads it from the directory REPOSITORY-DIRECTORY-PATHNAME.
Defaults to loading a java-user-project."
  (load-java-user-project project-name repository-directory-pathname))

(defmethod initialize-java-project ((self java-project))
  "Initializes the  global-scope, project-context, and the memoization-cache 
of the java-project instance SELF."
  (setf (global-scope self)
        (java-global-scope))
  (setf (project-context self)
        (java-project-context))
  (setf (memoization-cache self)
        (java-memoization-table (global-scope self) (project-context self))))

(defun load-java-project-from-file (relative-filename repository-directory-pathname deserialization-function)
  "Reads the project data that is stored as an association list
 in the file named FILENAME and creates a new project object.
The file named FILENAME has to be located in the directory named
REPOSITORY-DIRECTORY-PATHNAME."
  (declare #.*standard-optimize-settings*
           (type (or pathname string)
                 relative-filename repository-directory-pathname))
  (let ((alist ())
	(pathname (io.fad:relative-pathname-as-file repository-directory-pathname relative-filename))
	(instance nil))
    (with-open-file (file-stream pathname :direction :input)
      (handler-case
	  (setq alist
		(read-project-record file-stream))
	(error (condition)
	       (error "~A error in file: ~A" condition relative-filename))))
    (setq instance 
          (funcall deserialization-function alist repository-directory-pathname))
    (load-classname-dictionary instance (classname-dictionary-pathname instance))
    (initialize-java-project instance)
    instance))

(defmethod save-java-project-file ((self java-project))
  "Saves the java-project SELF to the appropriate project
file in the repository-directory.
Assumes a fully initialized java-user-project."
  (let ((project-filename (concatenate 'string +project-suffix+ (project-name self) +source-extension+)))
    (with-open-file (file-stream
                     (merge-pathnames project-filename (repository-pathname self))
                     :direction :output :if-exists :supersede)
      (write-java-project self file-stream))))

(defmethod write-java-project ((self java-project) output-stream)
 "Stores the project data in an association list and writes
it to the stream OUTPUT-STREAM."
 (declare (type stream output-stream))
 (let ((current-package *package*))
   (setq cl:*package* (find-package +LOAD-JAVA-PROJECT-IN-PACKAGE+))
   (pprint (cons 'project (serialize-project-data self)) output-stream)
   (setq cl:*package* current-package)
   t))

(defmethod serialize-project-data ((self java-project))
  "Serializes the information of the java-project SELF that should 
be written to the project-file into an association list."
  (let ((alist '()))
    (declare (type list alist))
    (push (cons 'project-name
                (project-name self))
          alist)
    (push (cons 'java-version
                (java-version self))
          alist)
    (push (cons 'project-directory
                (directory-name-relative-to-repository self (project-pathname self)))
          alist)
    (push (cons 'project-library-directory
                (directory-name-relative-to-repository self (project-lib-pathname self)))
          alist)
    (push (cons 'project-analysis-directory
                (directory-name-relative-to-repository self (analysis-pathname self)))
          alist)
    (push (cons 'project-jar-file
                (jar-file self))
          alist)
    (push (cons 'project-classname-dictionary-file
                (directory-name-relative-to-repository self (classname-dictionary-pathname self)))
          alist)
    (push (cons 'project-debug-information-file
                (directory-name-relative-to-repository self (debug-information-pathname self)))
          alist)
    (push (cons 'project-library-jar-files
                (lib-jar-files self))
          alist)
    (push (cons 'project-analysis-filename-extensions
                (list (analysis-filename-extensions self)))
          alist)
    (push (cons 'project-classes
                (class-names self))
          alist)
    (nreverse alist)))

;; class loading   

(defmethod java-class ((self java-project) fully-qualified-classname)
  "RETURNS the jana-class that has the classname FULLY-QUALIFIED-CLASSNAME.
If no such class has been loaded yet, it is loaded from the filesystem."
  (declare #.*standard-optimize-settings*)
  (let ((classifier-declaration (gethash fully-qualified-classname
                                         (classes self))))
    (unless classifier-declaration
      (load-java-class self fully-qualified-classname)
      (setq classifier-declaration
            (gethash fully-qualified-classname
                     (classes self))))
    classifier-declaration))

(defmethod update-transformed-classes ((self java-project))
  "Creates a new transformed-class-names list consisting of all the
fully-qualified names of jana-classes (from both Java aspects and classes)
in the java-project SELF."
  (declare #.*standard-optimize-settings*)
  (let ((transformed-classes '()))
    (loop
        :for class-name :being :the :hash-keys :in (aspects self)
        :when (find class-name (aspect-names self) :test #'equal)
          :do (push class-name transformed-classes))
    (loop
        :for class-name :being :the :hash-keys :in (classes self)
        :when (find class-name (class-names self) :test #'equal)
          :do (push class-name transformed-classes))
    (setf (transformed-class-names self)
          (nreverse transformed-classes))))


;;; Java class metamodel loading

;; filesystem interface
(defmethod load-classname-dictionary ((self java-project)
				      (classname-dictionary-path pathname))
  "Reads the class-name-dictionary, which contains an association list
   that maps class-names to file-names from the file referenced byy
CLASSNAME-DICTIONARY-PATH. The association-list is stored in
the classname-dictionary of the project SELF."
  (declare #.*standard-optimize-settings*)
  (let ((alist ()))
    (when (debug-mode) 
      (format t "~% class-name dictionary path: ~A" classname-dictionary-path))
    (with-open-file (stream classname-dictionary-path
			    :direction :input
			    :if-does-not-exist nil)
      (when stream
	(setq alist (read stream))))
    (when (debug-mode)
      (format t "~% class-name dictionary: ~S" alist))
    (setf (classname-dictionary self)
	  alist)))

(defmethod translate-class-name ((self java-project) fully-qualified-classname)
  "RETURNS a file-name string or NIL if no corresponding jana class-file
is in the dictionary."
  (declare #.*standard-optimize-settings*)
  (let ((relative-filename
	 (cdr (assoc fully-qualified-classname
		     (classname-dictionary self)
		     :test #'equal))))
    (if relative-filename
        (io.fad:relative-pathname-as-file (repository-pathname self) relative-filename)
        nil)))

;; -------- DEBUG INFORMATION ----------

(defmethod save-debug-info-file ((self java-project))
  "Exports the debug-information that is part of the metamodel to
the file (debug-information-pathname self) of the java-project SELF."
  (declare #.*standard-optimize-settings*)
  (with-open-file (file-stream (debug-information-pathname self)
                               :direction :output :if-exists :supersede)
    (write-debug-information self file-stream)))

;; -------- MEMOIZATION -------

(defmacro with-memoization (project &body body)
  "uses the memoization-cache of the java-project PROJECT"
  `(let ((jana.java.base:*java-memoization-table*
          (jana.java:memoization-cache ,project)))
    ,@body))

;; --------- TESTING: IDENTITY TRANSFORMATION --------
         
(defun identity-transformation (project-name repository-directory-pathname)
  "Loads a Projects and all its classes, and saves it again.
The project named PROJECT-NAME is loaded from the repository-directory
REPOSITORY-DIRECTORY-PATHNAME."
  (declare #.*standard-optimize-settings*)
  (let ((project)
        (start 0)
        (end 0))
    (setq start (get-internal-real-time))
    (setq project
          (load-java-project project-name repository-directory-pathname))
    (load-java-classes project)
    (save-java-project project)
    (setq end (get-internal-real-time))
    (jana.base:print-time-elapsed start end :message "Loading & Saving finished after")))
