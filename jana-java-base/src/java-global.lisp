;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.base; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-global.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;   Synchronized Memoization-Table for all elements
;;;   of the Java Language.
;;;   The difference to scopes is, that the memoization tables
;;;   could in general store all kinds of instances independent of
;;;   their status in the Java language (e.g. package instances
;;;   which are no first-class elements in the Java language).
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Sat Sep  6 00:06:33 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:23:57 2010 (+0100)
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

(in-package :JANA.JAVA.BASE)

;;; chr: - the memoization tables use lists as keys, therefore keys have to be compared with #'equal
;;;      - note that not all implementations provide weakness for values!

(defclass java-signature-memoization-table ()
  ((jana-names
    :INITARG :jana-names
    :TYPE hash-table)
   (java-signatures
    :INITARG :java-signatures
    :TYPE hash-table))
  (:DOCUMENTATION "A cache for memoizing jana-names and java-signatures.
It maps strings to jana-signatures and jana-names"))
  

(defun java-signature-memoization-table ()
  "Constructor"
  (make-instance 'java-signature-memoization-table
                 :jana-names (make-value-weak-synchronous-hashtable :size 511 :test 'equal)
                 :java-signatures (make-value-weak-synchronous-hashtable :size 31 :test 'equal)))


(defclass java-local-memoization-table ()
  ((java-classifier-declarations
     :ACCESSOR java-classifier-declarations
     :TYPE hash-table
     :DOCUMENTATION "maps fully-qualified-name -> classifier-declaration")
   (java-fields
    :ACCESSOR java-fields-cache
    :TYPE hash-table
    :DOCUMENTATION "elements of type java-field-declaration")
   (java-modifiers
    :ACCESSOR java-modifiers-cache
    :TYPE hash-table
    :DOCUMENTATION "elements of type java-modifier")
   (java-values
    :ACCESSOR java-values-cache
    :TYPE hash-table
    :DOCUMENTATION "elements of type java-value")
   (java-local-variable-declarations
    :ACCESSOR java-local-variable-declarations-cache
    :TYPE hash-table
    :DOCUMENTATION "elements of type jimple-local-variable-declaration"))
  (:DOCUMENTATION "A memoization table for metamodel-elements with local scope."))

(defun java-local-memoization-table ()
  "Constructor"
  (let ((instance (make-instance 'java-local-memoization-table)))
    (setf (java-classifier-declarations instance)
          (make-value-weak-synchronous-hashtable :size 31 :test 'equal))
    (setf (java-fields-cache instance)
	  (make-value-weak-synchronous-hashtable :test 'equal))
    (setf (java-modifiers-cache instance)
	  (make-value-weak-synchronous-hashtable :size 15 :test 'equal))
    (setf (java-values-cache instance)
	  (make-value-weak-synchronous-hashtable :size 61 :test 'equal))
    (setf (java-local-variable-declarations-cache instance)
	  (make-value-weak-synchronous-hashtable :size 511 :test 'equal))
    instance))


(defclass java-memoization-table (jana-memoization-table)
  ((signature-cache
    :ACCESSOR signature-cache
    :TYPE java-signature-memoization-table
    :DOCUMENTATION "a map from strings to signatures and names.")
   (global-scope
    :ACCESSOR global-scope
    :TYPE java-global-scope
    :DOCUMENTATION "The global scope in which types are visible.")
   (project-context
    :ACCESSOR project-context
    :TYPE java-project-context
    :DOCUMENTATION "The static context consisting of the packages used in a project.")
   (local-cache
    :ACCESSOR local-cache
    :TYPE java-local-memoization-table
    :DOCUMENTATION "A map from strings to metamodel elements with local scope, which is used during metamodel construction."))
  (:DOCUMENTATION "The java-memoization-table memoizes elements
of the Java programming language and signatures of these elements."))

(defun java-memoization-table (global-scope project-context)
  "Constructor"
  (declare (type java-global-scope global-scope)
           (type java-project-context project-context))
  (let ((instance (make-instance 'java-memoization-table)))
    (setf (global-scope instance)
          global-scope)
    (setf (project-context instance)
	  project-context)
    (setf (local-cache instance)
          (java-local-memoization-table))
    (setf (signature-cache instance)
	  (java-signature-memoization-table))
  instance))


(defmacro mmake-instance-using-env (java-memoization-table
				    environment-slotname
				    constructor keys)
  "Creates a memoizing constructor that uses the hashtable
stored at ENVIRONMENT-SLOTNAME in JAVA-MEMOIZATION-TABLE
to look-up already created instances.
New instances are created by calling the function CONSTRUCTOR.
If an instance already exists is determined using the objects in the list
KEYS."
  (unless keys
    (warn "No keys were specified with the keyword :keys !"))
  (unless constructor
    (warn "No constructor was specified using the keyword :constructor !"))
  (unless java-memoization-table
    (warn "No java-memoization-table was specified with the keyword :java-memoization-table !"))
  `(let* ((hashtable (slot-value ,java-memoization-table
		      ,environment-slotname))
	  ,(if (= (length keys) 1)
	       `(key ,@keys)
	       `(key (list ,@keys)))
	  (instance (gethash key hashtable)))
    ,(when (debug-mode)
	   `(if instance (format t "[mem]") (format t "[new]")))
    (unless instance
      (setq instance ,constructor)
      (setf (gethash key hashtable)
	    instance))
    instance))

(defmacro mmake-java-signature (&key ((:constructor constructor))
				     ((:java-memoization-table java-memoization-table))
				     ((:keys keys)))
  "Create a memoizing constructor using CONSTRUCTOR for
java-signature entries in the signature-cache of
JAVA-MEMOIZATION-TABLE. Instances are compared using KEYS."
  `(mmake-instance-using-env
    (slot-value ,java-memoization-table 'signature-cache)
    'java-signatures
    ,constructor ,keys))

(defmacro mmake-jana-name (&key ((:constructor constructor))
				((:java-memoization-table java-memoization-table))
				((:keys keys)))
  "Create a memoizing constructor using CONSTRUCTOR for
jana-name entries in the signature-cache of
JAVA-MEMOIZATION-TABLE. Instances are compared using KEYS."
  `(mmake-instance-using-env
    (slot-value ,java-memoization-table 'signature-cache)
    'jana-names
    ,constructor ,keys))

(defmacro mmake-java-package
    (&key ((:constructor constructor))
	  ((:java-memoization-table java-memoization-table))
	  ((:keys keys)))
  "Create a memoizing constructor using CONSTRUCTOR for java-classifier entries
in JAVA-MEMOIZATION-TABLE. Instances are compared using KEYS."
  `(mmake-instance-using-env (slot-value ,java-memoization-table 'project-context)
                             'java-packages
			     ,constructor ,keys))

(defmacro mmake-java-type (&key ((:constructor constructor))
				((:java-memoization-table java-memoization-table))
				((:keys keys)))
  "Create a memoizinrojg constructor using CONSTRUCTOR for java-type entries
in the global-scope of JAVA-MEMOIZATION-TABLE. Instances are compared using KEYS."
  `(mmake-instance-using-env (slot-value ,java-memoization-table 'global-scope)
                             'java-types
                              ,constructor ,keys))

(defmacro mmake-java-classifier-declaration
    (&key ((:constructor constructor))
	  ((:java-memoization-table java-memoization-table))
	  ((:keys keys)))
  "Create a memoizing constructor using CONSTRUCTOR for java-classifier entries
in the local-cache of JAVA-MEMOIZATION-TABLE. Instances are compared using KEYS."
  `(mmake-instance-using-env (slot-value ,java-memoization-table 'local-cache)
                             'java-classifier-declarations
			     ,constructor ,keys))

(defmacro mmake-java-modifier (&key ((:constructor constructor))
				    ((:java-memoization-table java-memoization-table))
				    ((:keys keys)))
  "Create a memoizing constructor using CONSTRUCTOR for java-modifier entries
in the local-cache of JAVA-MEMOIZATION-TABLE. Instances are compared using KEYS."
  `(mmake-instance-using-env (slot-value ,java-memoization-table 'local-cache)
                             'java-modifiers
			     ,constructor ,keys))

(defmacro mmake-java-field-declaration (&key ((:constructor constructor))
					     ((:java-memoization-table java-memoization-table))
					     ((:keys keys)))
  "Create a memoizing constructor using CONSTRUCTOR for java-field-declaration entries
in the local-cache of JAVA-MEMOIZATION-TABLE. Instances are compared using KEYS."
  `(mmake-instance-using-env (slot-value ,java-memoization-table 'local-cache)
                             'java-fields
			     ,constructor ,keys))

(defmacro mmake-java-value (&key ((:constructor constructor))
				 ((:java-memoization-table java-memoization-table))
				 ((:keys keys)))
  "Create a memoizing constructor using CONSTRUCTOR for java-value entries
in the local-cache of JAVA-MEMOIZATION-TABLE. Instances are compared using KEYS."
  `(mmake-instance-using-env (slot-value ,java-memoization-table 'local-cache)
                             'java-values
			     ,constructor ,keys))

(defmacro mmake-java-local-variable-declaration (&key
						 ((:constructor constructor))
						 ((:java-memoization-table java-memoization-table))
						 ((:keys keys)))
  "Create a memoizing constructor using CONSTRUCTOR for
java-local-variable-declaration entries in the local-cache of JAVA-MEMOIZATION-TABLE.
Instances are compared using KEYS."
  `(mmake-instance-using-env (slot-value ,java-memoization-table 'local-cache)
                             'java-local-variable-declarations
			     ,constructor ,keys))


;(let ((a "Test")) (macroexpand-1 '(mmake-jana-name :constructor (make-instance 'jana-name :name a) :java-memoization-table *java-memoization-table* :keys (a))))

;; some nice things that are possible using a structured global environment

;(defmethod undeclared-object-types ((self java-memoization-table))
;  "Returns a list with the signatures of undeclared object types"
;  (let ((declared-object-types (java-classifier-declarations-cache self))
;        (undeclared-object-types ()))
;    (maphash #'(lambda (key value)
;                 (when (subtypep (class-of value) 'java-object-reference-type)
;                   (unless (gethash key declared-object-types)
;                     (push (signature value) undeclared-object-types))))
;             (types self))
;    (nreverse undeclared-object-types)))

;(defmethod load-undeclared-object-types ((self java-memoization-table))
;  (let ((types-to-load ())
;        (loaded-types ()))
;    (setq types-to-load
;          (union types-to-load (undeclared-object-types self)))
;    (format t "Loading ~A undeclared object types." (length types-to-load))
;    (loop
;        :while (> (length types-to-load) 0)
;        :do
;              (format t "~%Loading ~A" (qualified-name (first types-to-load)))
;              (push (load-java-class (qualified-name (pop types-to-load)))
;                    loaded-types)
;            
;          ;(setq types-to-load
;          ;     (union types-to-load (undeclared-object-types self)))
;      )))