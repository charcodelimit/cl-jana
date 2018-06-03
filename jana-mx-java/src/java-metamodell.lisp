;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-metamodell.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Tue Jun 30 10:02:03 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:30:17 2010 (+0100)
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

#.(declaim (inline subclass-p))
(defun subclass-p (java-classifier-declaration super-class-qualified-name)
  "Checks if the class with signature super-class-qualified-name is a super-type
of the java-classifier-declaration JAVA-CLASSIFIER-DECLARATION."
  (let ((found nil))
    (declare (type boolean found))
    (if (equal (qualified-name java-classifier-declaration)
               super-class-qualified-name)
        t
        (dolist (super-type (super-types java-classifier-declaration) found)     
          (when (equal (qualified-name super-type)
                       super-class-qualified-name)
            (setq found t)
            (return))))))

#.(declaim (inline find-java-method-in-list))
(defun find-java-method-in-list (method-list method-name)
  (declare #.*standard-optimize-settings*
           (type list method-list)
           (type string method-name))
  (dolist (method method-list nil)
    (when (and (typep method 'java-method-implementation)
               (equal method-name
                      (unqualified-name method)))
      (return method))))

(defmethod direct-superclass ((self java-classifier-declaration) project)
  "RETURNS the java-classifier-declaration for the direct-superclass of
the class-declaration SELF. The class is loaded from the java-project PROJECT."
  (declare #.*standard-optimize-settings*)
  (if (super-types self)
      (java-class project (qualified-name (first (super-types self))))
      nil))

(defmethod find-java-method-in-class ((self java-classifier-declaration) method-name)
  "RETURNS the method-implementation of the method named METHOD-NAME
   when it is declared in the class SELF."
  (declare #.*standard-optimize-settings*)
  (find-java-method-in-list (methods self) method-name))

(defmethod find-java-method ((self java-classifier-declaration) method-name project)
  "RETURNS the method-implementation of the method named METHOD-NAME
if it is declared in the java class SELF or one of its super-classes."
  (declare #.*standard-optimize-settings*
           (type string method-name))
  (let ((super-class)
        (found-method nil))
    (setq found-method
          (find-java-method-in-list (methods self) method-name))
    (unless found-method
      (loop :for super-type :in (super-types self)
            :do (progn
                  (setq super-class
                        (java-class project (qualified-name super-type)))
                  (setq found-method
                        (find-java-method-in-list (methods super-class) method-name))
                  (when found-method
                    (return found-method)))))
    found-method))

;;; COPY CONSTRUCTORS

(defmethod copy-java-method-modifiers ((self java-method-modifiers))
  "RETURNS a new java-method-modifiers instance by copying the
modifier-list of the java-method-modifiers instance SELF."
  (declare #.*standard-optimize-settings*)
  (jana.java::make-java-modifiers 'java-method-modifiers (copy-list (modifier-list self))))

;;; chr: the safe way to implement copy constructors is to copy everything, as this guarantees that
;;;      the copied instance is not changed when the new instance is modified!
;;;      This version relies heavily on knowledge about the slot-values that are eventually changed,
;;;      i.e. only the modifiers.
;;;      The method name is also changed in the current implementation of the weaver, but not by
;;;      modifying the slot value.
(defmethod copy-java-method-implementation ((self java-method-implementation) &optional qualified-name)
  "Copy Constructor creates a copy of SELF without body"
  (declare #.*standard-optimize-settings*)
  (let ((method-name))
    (if qualified-name
	(setq method-name
	      qualified-name)
	(setq method-name
	      (qualified-name self)))
    (when (verbose-mode)
      (format t "c"))
    (when (debug-mode)
      (format t "~% Method-Implementation: ~A(" method-name)
      (if (argument-types self)
	  (dolist-first-last (argument (argument-types self))
	   (format t "~A " (unqualified-name (signature argument)))
	   (format t "~A" (unqualified-name (signature argument)))))
      (format t ")"))
    (let ((instance))
      (setq instance (make-instance 'java-method-implementation
				    :owner-class (util.clos:copy (owner-class self))
				    :method-modifiers (copy-java-method-modifiers (method-modifiers self))
				    :signature (util.clos:copy (java-signature method-name))
				    :return-type (util.clos:copy (return-type self))
				    :argument-types (util.clos:copy (argument-types self))
				    :thrown-exceptions (util.clos:copy (thrown-exceptions self))))
      (setf (annotations instance)
            (util.clos:copy (annotations self)))
      (setf (body instance)
            (make-instance 'java-closure))
      instance)))


(defmethod copy-java-type ((self java-type))
  "RETURNS a deep-copy of the java-type SELF."
  (util.clos:copy self))