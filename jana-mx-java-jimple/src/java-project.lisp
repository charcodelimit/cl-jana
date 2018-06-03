;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java.jimple; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-project.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;  Additional Methods at JIMPLE IR level for java-projects.
;;;  For example, counting of fields, and counting of field-access-sites.
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Tue Jul 21 11:34:48 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 22:27:58 2010 (+0100)
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

(in-package :JANA.MX.JAVA.JIMPLE)

;;
;; Example: (jana.mx.java.jimple::count-fields-in-project (jana.metamodel::load-project-file "project-tornado.lisp"))
;;
(defmethod count-fields-in-project ((self java-project))
  "Counts and DISPLAYS the number of fields declared in the classes of the java-project SELF."
  (declare #.*standard-optimize-settings*)  
  (let ((num-classes 0)
	(num-fields 0))
    (declare (type fixnum num-classes num-fields))
    (load-java-classes self)
    (maphash #'(lambda (class-name class)
		 (format t "~%~A" class-name)
		 (incf num-classes)
		 (setq num-fields (+ num-fields (length (fields class)))))
	     (classes self))
    (format t "~%~A fields in ~%~A classes." num-fields num-classes)))

;;
;; Example: (load "load-project")
;;          (jana.mx.java.jimple::count-fields-in-project (jana.metamodel::load-project-file "project-tornado.lisp"))
;;
;; ToDo: - create a method qualified-field-name or just qualified-name for JJavaField
;;       - simplify the nested conditions!
;;       
(defmethod count-field-accesses-in-project ((self java-project))
  "Counts the number of times that a field is accessed in classes
belonging to the project SELF.
The function keeps track of the counts for each individual field,
that is identified by its fully qualified name (e.g. java.lang.String.len).
RETURNS a hash-table containing the mapping between fully qualified field-names
and the number of times the field was accessed in the project."
  (declare #.*standard-optimize-settings*)
  (let ((field-map (make-hash-table :test 'equal))
	  (class nil)
	  (value nil))
      (load-java-classes self)
      (maphash #'(lambda (class-name class)
                   (let ((field-name ""))
                     (dolist (field (fields class))
                       (setq field-name (qualified-name field))
                       (setf (gethash
                              (concatenate 'string class-name "." field-name)
                              field-map)
                             0)
                       (format t "~%~A.~A" class-name field-name))))
	       (classes self))
      (dolist (class-name (class-names self))
	(setq class
	      (gethash class-name (classes self)))
	(dolist (method (methods class))
	  (when (typep method 'java-method-implementation)
	    (format t "~%Method: ~A.~A~% " class-name (qualified-name method))
	    (dolist (instruction (instructions (body method)))
	      (cond
		((typep instruction 'jimple-imaginary-instruction-assignment)
		 (setq value (assignment-source instruction))
		 (if (typep value 'jimple-reference-value-field)
		     (progn
		       ;(format t "R[~A.~A]" (qualified-name (field-owner value)) (field-name value))
		       (when (gethash
			      (concatenate 'string (qualified-name (field-owner value)) "." (field-name value))
			      field-map)
			 (incf (gethash
				(concatenate 'string (qualified-name (field-owner value)) "." (field-name value))
				field-map))))
                     (format t "a"))
		 (setq value (assignment-target instruction))
		 (if (typep value 'jimple-reference-value-field)
		     (progn
		       ;(format t "W[~A.~A]" (qualified-name (field-owner value)) (field-name value))
		       (when (gethash
			      (concatenate 'string (qualified-name (field-owner value)) "." (field-name value))
			      field-map)
			 (incf (gethash
				(concatenate 'string (qualified-name (field-owner value)) "." (field-name value))
			      field-map))))
		     (format t "a")))
		(t
		 (format t "."))))
	    )))
      field-map))
   
(defun show-field-accesses-in-project (field-map)
  (let ((sum-field-accesses 0))
    (maphash #'(lambda (field-name field-accesses)
		 (incf sum-field-accesses field-accesses)
		 (format t "~%~A was accessed: ~A times." field-name field-accesses))
	     field-map)
    (format t "~%~%~A field accesses in total." sum-field-accesses)))

(defun write-field-accesses-in-project (output-stream field-map)
  (maphash #'(lambda (field-name field-accesses)
	       (princ field-name output-stream)
	       (princ " " output-stream)
	       (princ field-accesses output-stream)
	       (princ #\newline output-stream))
	   field-map))

(defun save-field-accesses-in-project (file-name field-map)
  (let ((path (merge-pathnames file-name)))
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-field-accesses-in-project stream field-map))))

(defmethod write-field-access-statistics-to-file ((self java-project) file-name)
  "Writes a table with the fully qualified field-names of all fields
declared in classes of the java-project SELF"
  (let ((field-map (count-field-accesses-in-project self)))
    (save-field-accesses-in-project file-name field-map)
    (show-field-accesses-in-project field-map)))


(defmethod count-constructors-with-loops-in-project ((self java-project))
  "Counts the number of constructors that contain loops in the
java-project SELF."
  (declare #.*standard-optimize-settings*)
  (let ((constructors-with-loops '())
        (has-loops nil)
        (class nil)
        (constructor-count 0)
        (own-index 0)
        (target-index 0))
    (declare (type list constructors-with-loops)
             (type fixnum constructor-count own-index target-index))
    (load-java-classes self)
    (dolist (class-name (class-names self))
      (setq class
            (gethash class-name (classes self)))
      (dolist (method (methods class))
        (when (and (typep method 'java-method-implementation)
                   (equal (qualified-name method) "<init>"))
	    (format t "~%Constructor: ~A.~A~% " class-name (qualified-name method))
            (incf constructor-count)
            (setq has-loops nil)
	    (dolist (instruction (instructions (body method)))
	      (cond
                ((typep instruction 'jimple-goto-instruction)
                 (setq own-index
                       (instruction-index instruction))
                 (setq target-index
                       (gethash (first (branch-targets instruction))
                                (branch-target-table (body method))))
                 (when (> (- own-index target-index) 0)
                   (setq has-loops t)
                   (format t "l")
                   (return)))
                (t
                 (format t "."))))
            (when has-loops
              (push method constructors-with-loops)))))
    (format t "~%")
    (dolist (constructor constructors-with-loops)
      (format t "~%~A.~A" (qualified-name (owner-class constructor)) (qualified-name constructor)))
    (format t "~%")
    (format t "~%Constructors in Project: ~A" constructor-count)
    (format t "~%Constructors with loops in Project: ~A" (length constructors-with-loops))
    (length constructors-with-loops)))