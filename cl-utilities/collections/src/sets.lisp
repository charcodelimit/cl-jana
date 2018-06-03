;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.collections; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        sets.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;  TODO: Hashsets in the current state do not work for arbitrary objects,
;;;        as no custom hash-function can be used in all implementations
;;;        Though, custom test functions are allowed in CCL and LISPWORKS.
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fri Jul 17 22:35:49 2009 (z)
;;; 
;;; Last-Updated: Mo Nov 23 23:51:28 2009 (+0100)
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

(in-package :UTIL.COLLECTIONS)

(defclass hash-set ()
  ((table
    :ACCESSOR hashset-table
    :TYPE generic-hash-table
    :INITFORM (make-synchronous-hashtable :test 'equalp)
    :INITARG :table
    :DOCUMENTATION "The hash-table used to store the elements in the set."))
  (:DOCUMENTATION "An unordered set. Adding and removing elements can be done in O(1), containment tests also take O(1).
Returning the list of all elements takes O(N) time."))
  
(defclass fast-hash-set (hash-set)
  ()
  (:DOCUMENTATION "A hash-set, where the identity of elements is determined
the function using eq."))

(defclass key-hash-set (hash-set)
  ((hash-key-function
    :READER hash-key
    :INITARG :key-test
    :INITFORM #'identity
    :TYPE function
    :DOCUMENTATION "The test-function that is used to test the equality of keys used for elements in the set."))
  (:DOCUMENTATION "A hash-set, where the identity of elements is determined by a test-function.
Element keys are calculated using the function hash-key-function.
If values have changed can be determined by supplying a function value-equality-test."))

(defclass bucket-hash-set (key-hash-set)
  ()
  (:DOCUMENTATION "A hash-set, that uses buckets to store values with the same hash-key, but different identity."))
  
(defclass generic-hash-set (key-hash-set)
  ((value-equality-test
    :READER value-test
    :INITARG :value-test
    :INITFORM nil
    :DOCUMENTATION "The test-function that is used to test the equality of elements in the set."))
  (:DOCUMENTATION "A hash-set that uses a test function in order to determine equality of keys,
and a hash-key function to determine the key for a value."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro make-hash-set (&rest keys)
    `(make-fast-hash-set ,@keys)))

(defun make-fast-hash-set (&rest keys &key (size 16) (test 'eq) &allow-other-keys)
  "Constructor."
  ;; process keys list
  (when (find :size keys)
    (remf keys :size))
  (when (find :test keys)
    (remf keys :test))
  (make-instance 'fast-hash-set
                 :table (make-synchronous-hashtable :size size :test test)))

(defun make-key-hash-set (&rest keys &key (size 16) (key-test #'equalp) (hash-key-function #'identity) &allow-other-keys)
  "Constructor."
  ;; process keys list
  (when (find :size keys)
    (remf keys :size))
  (when (find :key-test keys)
    (remf keys :key-test))
  (when (find :hash-key-function keys)
    (remf keys :hash-key-function))
  (make-instance 'key-hash-set
                 :table (make-synchronous-hashtable :size size :test key-test)
                 :key-test hash-key-function))

(defun make-bucket-hash-set (&rest keys &key (size 16) (key-test #'equal) (hash-key-function #'identity) &allow-other-keys)
  "Constructor."
  ;; process keys list
  (when (find :size keys)
    (remf keys :size))
  (when (find :key-test keys)
    (remf keys :key-test))
  (when (find :hash-key-function keys)
    (remf keys :hash-key-function))
  (make-instance 'bucket-hash-set
                 :table (make-synchronous-hashtable :size size :test key-test)
                 :key-test hash-key-function))

(defun make-generic-hash-set (&rest keys &key (size 16) (key-test #'equalp) (value-test nil) (hash-key-function #'identity) &allow-other-keys)
  "Constructor."
  ;; process keys list
  (when (find :size keys)
    (remf keys :size))
  (when (find :key-test keys)
    (remf keys :key-test))
  (when (find :value-test keys)
    (remf keys :value-test))  
  (when (find :hash-key-function keys)
    (remf keys :hash-key-function))
  (make-instance 'generic-hash-set
                 :table (make-synchronous-hashtable :size size :test key-test)
                 :key-test hash-key-function
                 :value-test value-test))


(defmethod copy-set ((self fast-hash-set))
  (make-instance 'fast-hash-set
                 :table (copy-hash-table (hashset-table self))))

(defmethod copy-set ((self bucket-hash-set))
  "Copies the hash-table containing the elements to new hash-set instance,
and sets the key-function."                           
  (make-instance 'bucket-hash-set
                 :table (copy-hash-table (hashset-table self)) 
                 :key-test (hash-key self)))

(defmethod copy-set ((self generic-hash-set))
  "Copies the hash-table containing the elements to new hash-set instance,
and sets the key-function."                           
  (make-instance 'generic-hash-set
                 :table (copy-hash-table (hashset-table self)) 
                 :key-test (hash-key self)
                 :value-test (value-test self)))


(defmethod add-element (value (set fast-hash-set))
  "Adds a value VALUE to the fast-hash-set SET,
when the key of value and an element with the same key in the set are not equal."
  (declare #.*standard-optimize-settings*)
  (cond ((nth-value 1 (gethash value (hashset-table set)))
         (values value nil))
        (t
         (setf (gethash value (hashset-table set))
               value)
         (values value t))))

(defmethod add-element (value (set bucket-hash-set))
 "Adds a value VALUE to the hash-set SET,
when the key of value and an element with the same key in the set are not equal."
  (declare #.*standard-optimize-settings*)
  (when value
    (let* ((key (funcall (hash-key set) value))
           (bucket (gethash key (hashset-table set))))
      (declare (type list bucket))
      (cond ((find value bucket)
             (values value nil))
            (t
             (push value
                   (gethash key (hashset-table set)))
             (values value t))))))

(defmethod add-element (value (set generic-hash-set))
  "Adds a value VALUE to the hash-set SET,
when the key of value and an element with the same key in the set are not equal."
  (declare #.*standard-optimize-settings*)
  (when value
    (let ((key (funcall (hash-key set) value)))
      (cond ((nth-value 1 (gethash key (hashset-table set)))
             (values value nil))
            (t
             (setf (gethash key (hashset-table set))
                   value)
             (values value t))))))

(defmethod replace-element (value (set fast-hash-set))
  "Replaces the value VALUE in the fast-hash-set SET."
  (declare #.*standard-optimize-settings*)
  (remhash value (hashset-table set))
  (setf (gethash value (hashset-table set))
        value))

(defmethod replace-element (value (set bucket-hash-set))
  "Replaces all values with the same hash-key as VALUE
in the bucket-hash-set SET."
  (declare #.*standard-optimize-settings*)
  (when value
    (let ((key (funcall (hash-key set) value)))
      (setf (gethash key (hashset-table set))
            (list value)))))

(defmethod replace-element (value (set generic-hash-set))
  "Replaces the value VALUE in the hash-set SET."
  (declare #.*standard-optimize-settings*)
  (when value
    (let ((key (funcall (hash-key set) value)))
      (remhash key (hashset-table set))
      (setf (gethash key (hashset-table set))
            value))))


(defmethod remove-element (value (set fast-hash-set))
  "Removes the value VALUE from the fast-hash-set SET."
  (declare #.*standard-optimize-settings*)
  (remhash value (hashset-table set)))

(defmethod remove-element (value (set bucket-hash-set))
  "Removes the value VALUE from the hash-set SET."  
  (declare #.*standard-optimize-settings*)
  (when value
    (let* ((key (funcall (hash-key set) value))
           (bucket (gethash key (hashset-table set))))
      (declare (type list bucket))
      (when bucket
        (remove value bucket)
        (unless bucket
          (remhash key (hashset-table set)))))))

(defmethod remove-element (value (set generic-hash-set))
  "Removes the value VALUE from the hash-set SET."
  (declare #.*standard-optimize-settings*)
  (when value
    (remhash (funcall (hash-key set) value)
             (hashset-table set))))


(defmethod remove-bucket (value (set bucket-hash-set))
  "Removes the bucket where the value VALUE is stored,
from the hash-set SET."  
  (declare #.*standard-optimize-settings*)
  (when value
    (remhash (funcall (hash-key set) value)
             (hashset-table set))))


(defmethod elements ((set hash-set))
  "RETURNS all items stored in the hash-set SET."
  (nreverse
   (loop :for element :being :the :hash-values :in (hashset-table set)
         :collect element)))

(defmethod elements ((set bucket-hash-set))
  "RETURNS all items stored in the hash-set SET."
  (loop :for bucket :being :the :hash-values :in (hashset-table set)
        :with elements = '()
        :do (loop :for element :in bucket
                  :do (push element elements))
        :finally (return elements)))


(defmethod hashset-count ((set hash-set))
  "RETURNS the number of items stored in the hash-set SET."
  (declare #.*standard-optimize-settings*)  
  (hash-table-count (hashset-table set)))

(defmethod hashset-count ((set bucket-hash-set))
  "RETURNS the number of items stored in the hash-set SET."
  (declare #.*standard-optimize-settings*)
  (loop :for bucket :being :the :hash-values :in (hashset-table set)
        :with count = 0
        :do (incf count 
                  (length bucket))
        :finally (return count)))

;;; set operations

(defmethod hashset-union ((set-1 fast-hash-set) (set-2 fast-hash-set))
  "RETURNS a new fast-hash-set containing the union
of the fast-hash-sets SET-1 and SET-2."
  (declare #.*standard-optimize-settings*)
  (let ((tmp-table (copy-hash-table (hashset-table set-1))))
    (declare (type hash-table tmp-table))
    (loop :for element :being :the :hash-values :in (hashset-table set-2)
          :do (unless (gethash element tmp-table)
                (setf (gethash element tmp-table)
                      element)))
    (make-instance 'fast-hash-set
                   :table tmp-table)))

(defmethod hashset-union ((set-1 bucket-hash-set) (set-2 bucket-hash-set))
  (declare #.*standard-optimize-settings*)
  (let ((tmp-table (copy-hash-table (hashset-table set-1)))
        (key)
        (element))
    (declare (type hash-table tmp-table))
    (loop :for bucket :being :the :hash-values :in (hashset-table set-2)
          :do  (setq key
                     (funcall (hash-key set-1) (first bucket)))
               (setq element
                     (gethash key tmp-table))
               (setf (gethash key tmp-table)
                     (or element
                         (union bucket element))))
    (make-instance 'generic-hash-set
                   :table tmp-table
                   :key-test (hash-key set-1))))

(defmethod hashset-union ((set-1 generic-hash-set) (set-2 generic-hash-set))
  (declare #.*standard-optimize-settings*)
  (let ((tmp-table (copy-hash-table (hashset-table set-1)))
        (value)
        (key))
    (declare (type hash-table tmp-table))
    (loop :for element :being :the :hash-values :in (hashset-table set-2)
          :do  (setq key
                     (funcall (hash-key set-1) element))
               (setq value
                     (gethash key tmp-table))
               (unless (and value
                            (or (eq (value-test set-1) nil)
                                (funcall (value-test set-1) value element)))
                 (setf (gethash key tmp-table) element)))
    (make-instance 'generic-hash-set
                   :table tmp-table
                   :key-test (hash-key set-1)
                   :value-test (value-test set-1))))


(defmethod hashset-nunion ((set-1 fast-hash-set) (set-2 fast-hash-set))
  "RETURNS the union of the fast-hash-sets SET-1 and SET-2.
This function modifies the fast-hash-set SET-1."
  (declare #.*standard-optimize-settings*)
  (let ((tmp-table (hashset-table set-1)))
    (declare (type hash-table tmp-table))
    (loop :for element :being :the :hash-values :in (hashset-table set-2)
          :do (unless (gethash element tmp-table)
                (setf (gethash element tmp-table)
                      element)))
    set-1))

(defmethod hashset-nunion ((set-1 bucket-hash-set) (set-2 bucket-hash-set))
  (declare #.*standard-optimize-settings*)
  (let ((tmp-table (hashset-table set-1))
        (element)        
        (key))
    (declare (type hash-table tmp-table))
    (loop :for bucket :being :the :hash-values :in (hashset-table set-2)
          :do  (setq key
                     (funcall (hash-key set-1) (first bucket)))
               (setq element
                     (gethash key tmp-table))
               (setf (gethash key tmp-table)
                     (or element
                         (union bucket element))))
    set-1))

(defmethod hashset-nunion ((set-1 generic-hash-set) (set-2 generic-hash-set))
  (declare #.*standard-optimize-settings*)
  (let ((tmp-table (hashset-table set-1))
        (changed '())
        (old-value)
        (key))
    (declare (type hash-table tmp-table)
             (type boolean changed))
    (loop :for element :being :the :hash-values :in (hashset-table set-2)
          :do  (setq key
                     (funcall (hash-key set-1) element))
               (setq old-value
                     (gethash key tmp-table))
               (unless (and old-value
                            (or (eq (value-test set-1) nil)
                                (funcall (value-test set-1) old-value element)))
                 (setq changed t)
                 (setf (gethash key tmp-table) element)))
    (values set-1 changed)))

;;; tests

(defmethod contains-value-p (value (set fast-hash-set))
  "RETURNS T if the fast-hash-set SET contains the value
VALUE. Otherwise, NIL is returned."
  (declare #.*standard-optimize-settings*)
  (nth-value 1 (gethash value (hashset-table set))))

(defmethod contains-value-p (value (set bucket-hash-set))
  "RETURNS T if the hash-set SET contains the value
VALUE. Otherwise, NIL is returned."
  (declare #.*standard-optimize-settings*)
  (let ((bucket (gethash (funcall (hash-key set) value) set)))        
    (find value bucket)))

(defmethod contains-value-p (value (set generic-hash-set))
  "RETURNS T if the hash-set SET contains the value
VALUE. Otherwise, NIL is returned."
  (declare #.*standard-optimize-settings*)
  (let ((found-value
         (nth-value 0 (gethash (funcall (hash-key set) value)
                               (hashset-table set)))))
    (if (eq (value-test set) nil)
        found-value
        (funcall (value-test set) value found-value))))

;;; iteration

(defmacro map-elements (function set)
  "The MAP equivalent for hash-set."
  `(maphash #'(lamda (key value)
               (funcall ,function value))
    (hashset-table ,set)))

(defmacro map-bucketset-elements (function set)
  "The MAP equivalent for hash-set."
  `(maphash #'(lamda (key bucket)
               (mapcar ,function bucket)) 
    (hashset-table ,set)))

(defmacro doset ((variable-symbol hash-set &optional result-form) &body body)
  "DOLIST like iteration over hash-set elements."
  `(loop :for ,variable-symbol :being :the :hash-values :in (hashset-table ,hash-set)
    :do ,@body
    :finally (return ,result-form)))

(defmacro do-bucketset ((variable-symbol hash-set &optional result-form) &body body)
  "DOLIST like iteration over hash-set elements."
  (let ((bucket-sym (gensym)))
    `(loop :for ,bucket-sym :being :the :hash-values :in (hashset-table ,hash-set)
      :do (loop :for ,variable-symbol :in ,bucket-sym
                :do ,@body)
      :finally (return ,result-form))))