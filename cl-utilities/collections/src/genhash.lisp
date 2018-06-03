;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-utilities-collections; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        genhash.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;  Generic hash-table implementation that allows user-defined
;;;  test-functions.
;;;
;;;  TODO: allow (optional) synchronization
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Tue Sep 15 10:29:19 2009 (z)
;;; 
;;; Last-Updated: Sun Oct  4 12:33:52 2009 (z)
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

(in-package :CL-UTILITIES-COLLECTIONS)

(defvar *initialized* nil)
(defvar *hash-test-map* (make-hash-table))
(defvar *designator-function-map* (make-hash-table))

(define-condition hash-exists (simple-error) ()
  (:default-initargs
   :format-control "Hash table type ~a already registered"))

(define-condition unknown-hash (simple-error) ()
  (:default-initargs
   :format-control "Unknown hash table type ~a"))


(defclass hash-test ()
  ((test-designator :reader test-designator :initarg :test-designator :type (or function symbol)
                    :documentation "A symbol designating a hash-test.")
   (hash-function :reader hash-function :initarg :hash-function :type function
                  :documentation "The hash-function.")
   (eq-test :reader eq-test :initarg :eq-test :type function
            :documentation "The function used for equality testing.")
   (builtin :reader builtin :initarg :builtin :initform nil :type boolean
            :documentation "A boolean indicating if the test-designator is built-in into ANSI-CL hash-tables.")))

(defclass hash-container ()
  ((buckets :accessor buckets :initarg :buckets)
   (allocated-buckets :accessor allocated-buckets :initarg :allocated-buckets)
   (used-buckets :accessor used-buckets :initform 0)
   (stored-items :accessor stored-items :initarg :stored-items) 
   (test-designator :reader test-designator :initarg :test-designator)))


#.(declaim (inline get-test-function))
(defun get-test-function (test-designator)
  "Returns the test-function corresponding to a test-designator."
  (declare #.*standard-optimize-settings*)
  (cond ((symbolp test-designator)
         (gethash test-designator *designator-function-map*))
        ((functionp test-designator)
         test-designator)
        (t
         (error "The test-designator ~A must be a function or symbol!" test-designator))))

(defun add-designator-function-mapping (test-designator equal-function)
  "Associates a test-designator symbol with an equal-function"
  (setf (gethash test-designator *designator-function-map*)
        (if (symbolp equal-function)
            (symbol-function equal-function)
            equal-function)))

#.(declaim (inline get-hash-test))
(defun get-hash-test (test-designator)
  "RETURNS the hash-test object for the test-designator TEST-DESIGNATOR."
  (declare #.*standard-optimize-settings*)
  (gethash (get-test-function test-designator) *hash-test-map*))

#.(declaim (inline add-hash-test))
(defun add-hash-test (test-designator hash-test-object)
  "associates the test-designator TEST-DESIGNATOR with the hash-test object HASH-TEST-OBJECT."
  (declare #.*standard-optimize-settings*)
  (setf (gethash (get-test-function test-designator) *hash-test-map*)
        hash-test-object))

(defun register-test-designator (test-designator equal-function hash-function)
  "Associates a test-designator TEST-DESIGNATOR with the function EQUAL-FUNCTION
for testing equality, and a function HASH-FUNCTION for computing hash-values."
  ;; retrieve the functions if necessary
  (when (debug-mode)
    (format t "~% Registering hash-test-designator ~A" test-designator))
  (when (symbolp hash-function)
    (setq hash-function (symbol-function hash-function)))
  (when (symbolp equal-function)
    (setq equal-function (symbol-function equal-function)))
  ;; add designator->function mapping
  (add-designator-function-mapping test-designator equal-function)
  ;; handle existing hash-test designator
  (let ((hash-test (get-hash-test test-designator)))
    (when hash-test
      (unless (or (builtin hash-test)
                  (and (eql hash-function (hash-function hash-test))
                       (eql equal-function (eq-test hash-test))))
	(error 'hash-exists :format-arguments (list test-designator)))))
  ;; create hash-test and add designator->hash-test mapping
  (let ((hash-test (make-instance 'hash-test
                                  :test-designator test-designator
                                  :hash-function hash-function
                                  :eq-test equal-function)))
    (add-hash-test test-designator hash-test)))
;;
(defun register-hash-test (test-designator equal-function hash-function)
  "Registers for the test-designator TEST-DESIGNATOR a function EQUAL-FUNCTION
for testing equality, and a function HASH-FUNCTION for computing hash-values."
  (register-test-designator test-designator equal-function hash-function))


(defun register-builtin (test-designator)
  "Used to register built-in functions like #'cl:eq, or #'cl:equal."
  (if (symbolp test-designator)
      (add-designator-function-mapping test-designator (symbol-function test-designator))
      (add-designator-function-mapping test-designator test-designator))
  (add-hash-test test-designator
                 (make-instance 'hash-test :builtin t)))

(defun builtin-test-p (test-designator)
  "Tests if the symbol or function TEST-DESIGNATOR corresponds to a built-in test function, 
that is: #'eq, #'eql, #'equal, and #'equalp."
  (let ((hash-test (get-hash-test test-designator)))
    (and hash-test
         (builtin hash-test))))

(defun make-generic-hashtable (&key (size 17) (test #'eql))
  "Constructor"
  (make-generic-hash-table :size size :test test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Add, get and remove values

(defgeneric hashref (key table &optional default)
  (:DOCUMENTATION "The GETHASH equivalent for generic hash-tables.
RETURNS the value or NIL and a generalized-boolean indicating
if a value has been found in the hash-table TABLE for the key KEY."))
(defgeneric (setf hashref) (value key table &optional default))
(defgeneric hashrem (key table)
  (:DOCUMENTATION "The REMHASH equivalent for generic hash-tables.
REMOVES the key KEY from the generic hash-table TABLE."))
(defgeneric hashmap (key table)
  (:DOCUMENTATION "The MAPHASH equivalent for generic hash-tables"))


(defun expand-hash-table (table)
  (let* ((new-size (1+ (* 2 (allocated-buckets table))))
	 (new-buckets (make-array (list new-size) :initial-element nil)))
    (let ((old-data (buckets table)))
      (setf (allocated-buckets table) new-size)
      (setf (used-buckets table) 0)
      (setf (buckets table) new-buckets)
      (loop for bucket across old-data
	    do (loop for chunk in bucket
		     do (setf (hashref (car chunk) table) (cdr chunk))))))
  table)

(defmethod hashref (key (table hash-container) &optional default)
  (let ((hash-test (get-hash-test (test-designator table))))
    (let ((hash (funcall (hash-function hash-test) key)))
      (let ((bucket
	     (aref (buckets table) (mod hash (allocated-buckets table)))))
	(let ((data default) found (eqfun (eq-test hash-test)))
	  (flet ((check (chunk)
		   (when (funcall eqfun (car chunk) key)
		     (setf data (cdr chunk))
		     (setf found t))))
	    (loop for chunk in bucket
		  until found
		  do (check chunk))
	    (values data found)))))))

(defmethod hashref (key (table hash-table) &optional default)
  (gethash key table default))


(defmethod (setf hashref) (value key (table hash-container) &optional def)
  (declare (ignore def))
  (let ((hash-test (get-hash-test (test-designator table))))
    (if (= (allocated-buckets table) (used-buckets table))
	(expand-hash-table table))

    (let ((hash (funcall (hash-function hash-test) key))
	  (buckets (buckets table))
	  (size (allocated-buckets table)))
      (let* ((bucket-ix (mod hash size))
	     (bucket (aref buckets bucket-ix)))
	(if (null (aref buckets bucket-ix))
	    (progn
	      (setf (aref buckets bucket-ix)
		    (cons (cons key value) bucket))
	      (incf (used-buckets table))
	      (incf (stored-items table)))
	  (let ((check
		 (member key bucket
			 :key #'car :test (eq-test hash-test))))
	    (if check
		(setf (cdr (car check)) value)
	      (progn
		(setf (aref buckets bucket-ix)
		    (cons (cons key value) bucket))
		(incf (stored-items table)))))))))
  value)

(defmethod (setf hashref) (value key (table hash-table) &optional default)
  (declare (ignore default))
  (setf (gethash key table) value))


(defmethod hashrem (key (table hash-container))
  (when (hashref key table nil)
    (let ((hash-test (get-hash-test (test-designator table))))
      (let ((hash (funcall (hash-function hash-test) key))
	    (buckets (buckets table))
	    (size (allocated-buckets table)))
	(let* ((bucket-ix (mod hash size))
	       (bucket (aref buckets bucket-ix)))
	  (setf (aref buckets bucket-ix)
		(delete key bucket :test (eq-test hash-test) :key 'car))
	  (unless (aref buckets bucket-ix)
	    (decf (used-buckets table)))
	  (decf (stored-items table)))))
    t))

(defmethod hashrem (key (table hash-table))
  (remhash key table))


(defmethod hashclr ((table hash-container))
  (setf (used-buckets table) 0)
  (loop for ix from 0 below (allocated-buckets table)
	do (setf (aref (buckets table) ix) nil))
  table)

(defmethod hashclr ((table hash-table))
  (clrhash table))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Hash table iteration

(defmethod all-hash-keys ((table hash-container))
  (loop for list across (buckets table)
	append (mapcar #'car list)))

(defmethod all-hash-keys ((table hash-table))
  (loop for key being the hash-keys of table
	collect key))

(defmethod hashmap (fn (table hash-container))
  (let ((buckets (buckets table)))
    (loop for bucket across buckets
	  do (loop for chunk in bucket
		   do (funcall fn (car chunk) (cdr chunk))))))

(defmethod hashmap (fn (table hash-table))
  (maphash fn table))

(defmacro with-generic-hash-table-iterator ((name table) &body body)
  (let ((table-des (gensym "TABLE"))
	(the-keys (gensym "KEYS")))
    `(let ((,table-des ,table))
       (let ((,the-keys (all-hash-keys ,table-des)))
	 (macrolet ((,name ()
		     `(when ,the-keys
			  (prog1
			      (values t
				      (car ,the-keys)
				      (hashref (car ,the-keys) ,table-des))
			    (setf ,the-keys (cdr ,the-keys))))))
	   ,@body)))))

(defmacro dotable-builtin-keys ((variable-symbol hash-table &optional result-form) &body body)
  "Iterates over hash-table-keys"
  `(loop :for ,variable-symbol :being :the :hash-keys :in ,hash-table
    :do ,@body
    :finally (return ,result-form)))

(defmacro dotable-genhash-keys ((variable-symbol hash-table &optional result-form) &body body)
  `(let ((buckets (buckets ,hash-table))
          (,variable-symbol))
      (loop :for bucket :across buckets
            :do (loop :for chunk :in bucket
                      :do
                       (setq ,variable-symbol (car chunk))
                       ,@body)
            :finally (return ,result-form))))

(defmacro dotable-keys ((variable-symbol hash-table &optional result-form) &body body)
  #+(OR CCL LISPWORKS) `(dotable-builtin-keys (,variable-symbol ,hash-table ,result-form) ,@body)
  #-(OR CCL LISPWORKS) `(if (typep ,hash-table 'hash-table)
                                   ,`(dotable-builtin-keys (,variable-symbol ,hash-table ,result-form) ,@body)
                                   ,`(dotable-genhash-keys (,variable-symbol ,hash-table ,result-form) ,@body)))

(defmacro dotable-builtin-values ((variable-symbol hash-table &optional result-form) &body body)
  "Iterates over hash-table-values"
  `(loop :for ,variable-symbol :being :the :hash-values :in ,hash-table
    :do ,@body
    :finally (return ,result-form)))

(defmacro dotable-genhash-values ((variable-symbol hash-table &optional result-form) &body body)
  `(let ((buckets (buckets ,hash-table))
          (,variable-symbol))
      (loop :for bucket :across buckets
            :do (loop :for chunk :in bucket
                      :do
                       (setq ,variable-symbol (cdr chunk))
                       ,@body)
            :finally (return ,result-form))))

(defmacro dotable-values ((variable-symbol hash-table &optional result-form) &body body)
  #+(OR CCL LISPWORKS) `(dotable-builtin-values (,variable-symbol ,hash-table ,result-form) ,@body)
  #-(OR CCL LISPWORKS) `(if (typep ,hash-table 'hash-table)
                                   ,`(dotable-builtin-values (,variable-symbol ,hash-table ,result-form) ,@body)
                                   ,`(dotable-genhash-values (,variable-symbol ,hash-table ,result-form) ,@body)))


;;;;;;;;;;;;;;;;
;;; Hash table information
(defgeneric generic-hash-table-count (table))
(defgeneric generic-hash-table-size (table))
(defgeneric generic-hash-table-p (table))

(defmethod generic-hash-table-count ((table hash-container))
  (stored-items table))

(defmethod generic-hash-table-count ((table hash-table))
  (hash-table-count table))


(defmethod generic-hash-table-size ((table hash-container))
  (used-buckets table))

(defmethod generic-hash-table-size ((table hash-table))
  (hash-table-size table))


(defmethod generic-hash-table-p ((table t))
  nil)
(defmethod generic-hash-table-p ((table hash-container))
  t)
(defmethod generic-hash-table-p ((table hash-table))
  t)

;;;
;;; Setting up default hash tables
;;;
(unless *initialized*
  (setf *initialized* t)
  (register-hash-test 'cl:eq #'eq #'sxhash)
  (register-hash-test 'cl:eql #'eql #'sxhash)
  (register-hash-test 'cl:equal #'equal #'sxhash)
  (register-hash-test 'cl:equalp #'equalp #'sxhash)
  (register-builtin #'eq)
  (register-builtin #'eql)
  (register-builtin #'equal)
  (register-builtin #'equalp))