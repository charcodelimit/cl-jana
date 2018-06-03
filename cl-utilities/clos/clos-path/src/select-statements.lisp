;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.clos.path; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        select-statements.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;  The primitive query elements
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Jul  6 23:02:16 2009 (z)
;;; 
;;; Last-Updated: Tue Jul  7 16:30:24 2009 (z)
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

(in-package :util.clos.path)

#.(declaim (inline same-f))
(defun same-f (arg-list &key ((:test test-function) `#'equal))
  "RETURNS a closure that accepts one argument and evaluates
TEST-FUNCTION with the argument and the arguments given in ARG-LIST.
Example: (same-f () :test #'atom)
         => (lambda (#:G622) (apply #<FUNCTION ATOM> (list #:G622)))"
  (declare #.*standard-optimize-settings*
           (type list arg-list))
  (let ((arg (gensym)))
  `(lambda (,arg) (funcall ,test-function ,arg ,@arg-list))))

#.(declaim (inline sequence-subtypep))
(defun sequence-subtypep (object)
  "RETURNS T if the object is an instance
of a subtype of SEQUENCE and not a subtype of STRING."
  (declare #.*standard-optimize-settings*)
  (let ((object-class (class-of object)))
    (and (subtypep object-class 'sequence)
	 (not (subtypep object-class 'string)))))            

;; no recursive flattenning of sequences and hashmaps,
;; i.e. #(#(#(..))) => #(#(..))
#.(declaim (inline select-sequence-elements-f))
(defun select-sequence-elements-f (seq slot-name select-fn)
"RETURNS a vector with slot-values from the objects in sequence SEQ.
A predicate SELECT-FN is used to select slot-values. If SELECT-FN
valuates to T for a slot-value, then this slot-value is included in
the list of returned values."
;(format t "~%<DEBUG> [selecting-sequence-elements]")
  (declare #.*standard-optimize-settings*)
  (map 'vector
       #'(lambda (element) (elements element slot-name select-fn))
       seq))

;; no recursive flattenning of sequences and hashmaps,
;; i.e. #(#(#(..))) => #(#(..))
#.(declaim (inline select-hash-table-values-f))
(defun select-hash-table-values-f (htbl slot-name select-fn)
  "RETURNS a vector with slot-values from the objects that are the
values stored in hash-table HTBL.
A predicate SELECT-FN is used to select slot-values. If SELECT-FN
valuates to T for a slot-value, then this slot-value is included in
the list of returned values."
  (declare #.*standard-optimize-settings*
           (type hash-table htbl))
  ;(format t "~%<DEBUG> [selecting hash-table-values]")
  (let ((count 0)
	(values (make-sequence 'vector (hash-table-count htbl))))
    (declare (type fixnum count)
	     (type vector values))
    (loop
        :for value :being :the :hash-values :in htbl
        :do (setf (elt values count)
                  (elements value slot-name select-fn))
            (incf count))
    values))

;(weaver::element-f (weaver::element-f *a* "name" nil) "qualified-name" nil)
;; Responsibilities:
;;  - case-distinction based on slot-name and type of object
;;  - deal with lists of elements.
(defun element-f (object slot-name select-fn)
  "RETURNS selected elements of the slot SLOT-NAME in OBJECT,
if SLOT-NAME has a non-NIL value.
Otherwise the element(s) given by the argument OBJECT
are selected and returned.
A predicate SELECT-FN is used to select elements. If SELECT-FN
evaluates to T for an element that was given as argument,
then the element is included in the list of returned values.
If object is a vector, elements are selected by concatenating
the slot values of each element in the vector.
If object is a hash-table, elements are selected by concatenating
the slot values of each hash-table value."
  ; chr - documentation is too long, simplify this function!
  (declare #.*standard-optimize-settings*
           (type string slot-name))
  (cond
    ;; no slot name was given
    ((eq slot-name nil)
     (select select-fn object))
    ;; slot-name was given - object was a sequence 
    ((sequence-subtypep object)
     (if (> (length object) 0)
	 (reduce #'(lambda (x y) (concatenate 'vector x y))
		 (select-sequence-elements-f object slot-name select-fn))
	 (warn "Querying an empty sequence!~%~A" object)))
    ;; slot-name was given - object was a hash-table
    ((hash-table-p object)
     (if (> (hash-table-count object) 0)
	 (reduce #'(lambda (x y) (concatenate 'vector x y))
		 (select-hash-table-values-f object slot-name select-fn))
    	 (warn "Querying an empty hash-table!~%~A" object)))
    ;; else select elements from slot <slot-name> of instance <object>
    (t
     (elements object slot-name select-fn))))

#.(declaim (inline elements))
(defun elements (object slot-name select-fn)
  "RETURNS a list of elements from OBJECT that are accessible through
SLOT-NAME, and for which SELECT-FN returns T.
If both, an accessor and a slot, exist with the same name,
the accessor has precedence over direct access to the slot.
If no SELECT-FN is given, the values are returned.
Responsibilities: slot-value retrieval and selection"
  (declare #.*standard-optimize-settings*
           (type string slot-name)
	   (type standard-object object))
  (let ((slot-symbol (read-from-string slot-name)))
    (cond
      ; first try if its the name of an accessor function,
      ; because these have precedence (this is btw. also cheaper to test)
      ((fboundp slot-symbol)
       (select select-fn
	       (funcall slot-symbol object)))
      ; hmm.. maybe its the name of a slot
      ((slot-exists-p object slot-symbol)
       (select select-fn
	       (slot-value object slot-symbol)))
      (t
       (warn "There is no slot or accessor named: ~A [~A] defined for class: ~A !
Please do not forget to use the correct package prefix (e.g. cl-user::slotname)." slot-name slot-symbol (class-of object))))))

#.(declaim (inline collect-sequence-elements))
(defun collect-sequence-elements (seq)
"RETURNS a vector with slot-values from the objects in sequence SEQ."
  (declare (type sequence seq))
  ;(format t "~%<DEBUG> [collecting-sequence-elements]")
  (make-array (length seq) :initial-contents seq))

#.(declaim (inline collect-hash-table-values))
(defun collect-hash-table-values (htbl)
  "RETURNS a vector with slot-values from the objects that are the
values stored in hash-table HTBL."
  (declare #.*standard-optimize-settings*
           (type hash-table htbl))
  ;(format t "~%<DEBUG> [collecting hash-table-values]")
  (let ((count 0)
	(values (make-sequence 'vector (hash-table-count htbl))))
    (declare (type fixnum count)
	     (type vector values))
    (loop
        :for value :being :the :hash-values :in htbl
        :do (setf (elt values count)
                  value)
            (incf count))
    values))

;; the case distinction into objects, sequences, and hash-tables is needed,
;; because the head-object may be a collection type.
;; Select is consistent and always return a sequence ->
;; i.e. the empty sequence, the sequence containing one object,
;; or a sequence with multiple objects.
;; This consistence is not making the implementation slower,
;; returning a sequence is actually faster than returning an
;; object or sequence!
#.(declaim (inline select))
(defun select (select-fn object)
  "RETURNS a vector containing all elements from OBJECT for which
 the application of TEST-FUNCTION to ARG-LIST evaluates to T.
If OBJECT is no sequence, select behaves as if object is part of a
sequence containing a single element OBJECT.
If OBJECT is a sequence, it is not modified."
  (declare #.*standard-optimize-settings*)
  (let ((values (make-sequence 'vector 0)))
    (declare (type vector values))
    (cond ((sequence-subtypep object)
	   (setq values
		 (collect-sequence-elements object)))
	  ((hash-table-p object)
	   (setq values
		 (collect-hash-table-values object)))
	  (t
	   (setq values
		 (make-sequence 'vector 1 :initial-element object))))
    (if select-fn
	(select-from-vector select-fn values)
	values)))

;;  (time (dotimes (i 10000)
;;        (select-from-sequence #'evenp
;;                              '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
;;                                17 18 19 20))))
#.(declaim (inline select-from-sequence))
(defun select-from-vector (select-fn values)
  "RETURNS a sequence of VALUES for which SELECT-FN
evaluates to T."
  (declare (type function select-fn))
  (remove-if-not select-fn values))
   

