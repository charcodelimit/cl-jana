;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-loader.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;   Loading of jana-metamodel instances from the file-system.
;;;
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Jun 15 09:21:45 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:30:18 2010 (+0100)
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

;;; META-MODEL LOADING

(defgeneric load-java-class-from-file (project pathname)
  (:DOCUMENTATION "Loads the jana-metamodel representation of a Java class from the file-system."))

(declaim (inline eval-jana-java-metamodel))
(defun eval-jana-java-metamodel (list)
  "evaluates the jana-java-metamodel stored in LIST."
  (declare  #.*standard-optimize-settings*
            (type list list))
  (let (#+SBCL (sb-ext:*evaluator-mode* :interpret))
    (eval list)))

(defun load-java-metamodel (filename &key (file-type :probe))
  "Loads a Java language jana-metamodel from the file named by FILENAME.
The file-type indicator :FILE-TYPE can be one of :probe, or :uncompressed."
  (declare #.*standard-optimize-settings*)
  (let ((metamodel))
    (cond ((and (not (eql file-type :uncompressed)) (io.zip:is-gzip-file-p filename))
           (if (io.zip:is-supported-gzip-file-p filename)
               (setq metamodel
                     (eval-jana-java-metamodel (io.zip:read-gzip-file filename :package +METAMODEL-PACKAGE+)))
               (error "Unsupported GZip-File format!")))
          ((and (not (eql file-type :uncompressed)) (io.zip:is-valid-deflate-file-p filename))
           (if (io.zip:is-supported-deflate-file-p filename)
               (setq metamodel
                     (eval-jana-java-metamodel (io.zip:read-zlib-deflate-file filename :package +METAMODEL-PACKAGE+)))
               (error "Unsupported Deflate-File format!")))
          (t
           (setq metamodel
                 (eval-jana-java-metamodel (jana.metamodel:load-jana-metamodel filename :package +METAMODEL-PACKAGE+)))))
    metamodel))


(defmethod load-java-class-from-file ((self java-project) jana-class-file-pathname)
  "Loads the jana-metamodel found in the pathname element
JAVA-CLASS-FILE-PATHNAME."
  (declare #.*standard-optimize-settings*
           (type pathname jana-class-file-pathname))
  (let ((file-type :probe)
        (file-pathname)
        (compressed-file-pathname)
        (uncompressed-file-pathname))
    (setq compressed-file-pathname
          (make-pathname :type (compressed-filename-extension self) 
                         :defaults jana-class-file-pathname))
    (setq uncompressed-file-pathname
          (make-pathname :type (uncompressed-filename-extension self) 
                         :defaults jana-class-file-pathname))
    (cond 
      ((probe-file uncompressed-file-pathname)
       (setq file-pathname uncompressed-file-pathname)
       (setq file-type :uncompressed))
      ((probe-file compressed-file-pathname)
       (setq file-pathname compressed-file-pathname)
       (setq file-type :compressed))
      ((probe-file jana-class-file-pathname)
       (setq file-pathname jana-class-file-pathname)
       (setq file-type :probe))
      (t
       (setq file-pathname nil)))
    (cond 
      (file-pathname
       (let ((metamodel))
         (with-memoization self
           (if jana-class-file-pathname
               (setq metamodel (load-java-metamodel file-pathname :file-type file-type))
               (error "While loading Java class. Could not load file ~A"
                      (file-namestring jana-class-file-pathname)))
           (jana.java:initialize-subclasses metamodel (project-context jana.java.base:*java-memoization-table*))
           (jana.java:initialize-implementors metamodel (project-context jana.java.base:*java-memoization-table*)))
         metamodel))
      (t
       (warn "While loading Java class. Could not find file ~A" (file-namestring jana-class-file-pathname))
       nil))))

#.(declaim (inline load-java-class-silently))
(defmethod load-java-class ((self java-project) fully-qualified-classname &key (error-type #'error))
  "Loads a Java class from the repository and adds it to the project.
No output to the console is made other than warnings or errors.
Note: This method loads an already loaded class again!"
  (declare #.*standard-optimize-settings*)  
  (let ((pathname (jana.java::translate-class-name self fully-qualified-classname)))
    (if pathname
        (setf (gethash fully-qualified-classname (classes self))
              (load-java-class-from-file self pathname))
        (funcall error-type "Can't find class ~A. Please analyze the project first!" fully-qualified-classname))))

;;; loading of java-class metamodel instances
(defmethod load-java-class-with-progress ((self java-project) fully-qualified-classname &key (error-type #'error))
  "Loads a Java class from the repository, adds it to the projec, and prints .status information.
Note: This method loads an already loaded class again!"
  (declare #.*standard-optimize-settings*)
  (format t "~%~A" fully-qualified-classname)
  (load-java-class self fully-qualified-classname :error-type error-type))

#.(declaim (inline all-classes-loaded-p))
(defmethod all-classes-loaded-p ((self java-project))
  "RETURNS T if all classes defined in the java-project SELF were loaded."
  (declare #.*standard-optimize-settings*)
  (= (hash-table-count (classes self))
     (length (class-names self))))

#.(declaim (inline assert-classes-loaded))
(defmethod assert-classes-loaded ((self java-project))
  "ASSERTS that all classes in the java-project SELF were loaded."
  (assert (all-classes-loaded-p self)
          ()
          "Error while loading classes! The project contains ~A classes, but only ~A classes were loaded!"
          (length (class-names self)) (hash-table-count (classes self))))

(defmethod load-classes-serially ((self java-project) slot-name-class-names slot-name-class-list)
  "Creates the metamodel-instances for the classes corresponding to the class-names
stored in the slot with name SLOT-NAME-CLASS-NAMES in the java-project SELF.
The loaded created instances are stored at the slot named SLOT-NAME-CLASS-LIST in the java-project SELF."
  (declare #.*standard-optimize-settings*)
  (let ((load-function '()))
    (declare (type list load-function))
    (format t "~%[Loading Serially]")    
    (dolist (class-name (slot-value self slot-name-class-names))
      (unless (gethash class-name (slot-value self slot-name-class-list))
        (setq load-function
              `(lambda () (load-java-class-with-progress ,self ,(copy-seq class-name))))
        (funcall (eval load-function))))
    (assert-classes-loaded self)))

(defmethod load-classes-concurrently ((self java-project) slot-name-class-names slot-name-class-list)
  "Creates the metamodel-instances for the classes corresponding to the class-names
stored in the slot with name SLOT-NAME-CLASS-NAMES in the java-project SELF.
The loaded created instances are stored at the slot named SLOT-NAME-CLASS-LIST in the java-project SELF."
  (declare #.*standard-optimize-settings*)
  (let ((load-function)
        (task-list '()))
    (declare (type list load-function task-list))
    (format t "~%[Loading Concurrently]")
    ;;(setf eager-future:%thread-pool-soft-limit 3)
    (pcall:with-local-thread-pool (:size 2)
      (dolist (class-name (slot-value self slot-name-class-names))
        (unless (gethash class-name (slot-value self slot-name-class-list))
          (setq load-function
                `(lambda () (load-java-class-with-progress ,self ,(copy-seq class-name))))
          ;;(push (eager-future:pcall (eval load-function)) task-list)))
          (push (pcall:pcall (eval load-function)) task-list)))
      (dolist (task task-list)
        ;;(eager-future:yield task))
        (pcall:join task)))
    (assert-classes-loaded self)))

;;; Public Interface
(defmethod load-java-classes ((self java-project) &key (error-type #'warn))
  "creates the metamodel-instances for all classes defined in the project SELF.
Loading classes lazily instead with
\(load-java-class project fully-qualified-class-name\) is recommended!"
  (declare #.*standard-optimize-settings*)
  (let ((load-function #'load-classes-serially))
    (when (and jana.base:*multiprocessing* (not (debug-mode)))
      #+(OR CCL SBCL)  (setq load-function #'load-classes-concurrently))
    (if (> (length (classname-dictionary self)) 0)
        (progn
          (funcall load-function self 'jana.java:class-names 'jana.java::jana-classes)
          (funcall load-function self 'jana.java:aspect-names 'jana.java::jana-aspects))
        (funcall error-type "No classes to load. Please analyze the project first!"))))
