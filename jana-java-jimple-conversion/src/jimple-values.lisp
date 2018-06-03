;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple.conversion; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;;FILE:               jimple-values.lisp
;;;LANGUAGE:           Common-Lisp
;;;
;;;DESCRIPTION
;;;    
;;;    Conversion of the metamodel for the Jimple Intermediate Representation
;;;    of the Java Language into Jimple statements.
;;;    Representation of Values.
;;;
;;;    This is a translation of the CLOS based implementation of the representation
;;;    of values in the Jimple Intermediate Language for Java into statements
;;;    of the Jimple Intermediate Language.
;;;
;;; Author: Christian Hofmann
;;;
;;; Created: Fri Aug  8 23:19:00 2008 (z)
;;;
;;; Last-Updated: Mi Jan  6 18:25:29 2010 (+0100)
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
;;;****************************************************************************

(in-package :JANA.JAVA.JIMPLE.CONVERSION)

(defmethod write-jimple-statement ((self jimple-reference-value-local) stream)
  "RETURNS the string representation of a jimple-reference-value-array that is used in the Jimple intermediate language. This is just the name of the local variable."
  (declare (type stream stream))
  (write-sequence (variable-name self) stream))

(defmethod write-jimple-statement ((self jimple-reference-value-field) stream)
  "RETURNS the string representation of a jimple-reference-value-field that is used in the Jimple intermediate language.
These have the form: '<'<owner-class>': '<field-type>' '<field-name>'>'"
  (declare (type stream stream))
  (write-char #\< stream)
  (write-sequence (qualified-name (field-owner self)) stream)
  (write-char #\: stream)
  (write-char #\space stream)
  (write-jimple-statement (jana-type self) stream)
  (write-char #\space stream)
  (write-sequence (quote-name (field-name self)) stream)
  (write-char #\> stream))

(defmethod write-jimple-statement ((self jimple-reference-value-instance-variable) stream)
  "RETURNS the string representation of a jimple-reference-value-instance-variable
that is used in the Jimple intermediate language.
These have the form: <variable-name>'.''<'<owner-class>': '<field-type>' ''\"'<field-name>'\"''>'"
  (declare (type stream stream))
  (write-jimple-statement (local-variable-instance-reference self) stream)
  (write-char #\. stream)
  (call-next-method))

(defmethod write-jimple-statement ((self jimple-reference-value-array) stream)
  "RETURNS the string representation of a jimple-reference-value-array that is used in the Jimple intermediate language.
These have the form: <local-variable>'['<value>']'"
  (declare (type stream stream))
  (write-jimple-statement (local-variable-instance-reference self)
			  stream)
  (write-char #\[ stream)
  (write-jimple-statement (index self) stream)
  (write-char #\] stream))

(defmethod write-jimple-statement ((self jimple-reference-value-this) stream)
  "RETURNS the string representation of a jimple-reference-value-this that is used in the Jimple intermediate language. These have the form: '@this: '<reference-type>."
  (declare (type stream stream))
  (write-sequence "@this: " stream)
  (write-jimple-statement (jana-type self) stream))

(defmethod write-jimple-statement ((self jimple-reference-value-argument) stream)
  "RETURNS the string representation of a jimple-reference-value-argument that is used in the Jimple intermediate language.
These have the form '@parameter'<Index>': '<type>"
  (declare (type stream stream))
  (write-sequence "@parameter" stream)
  (princ (index self) stream)
  (write-char #\: stream)
  (write-char #\space stream)
  (write-jimple-statement (jana-type self) stream))

(defmethod write-jimple-statement ((self jimple-reference-value-caught-exception) stream)
   "RETURNS the string representation of a jimple-reference-value-caught-exception that is used in the Jimple intermediate language."
  (declare (type stream stream))
  (write-sequence "@caughtexception" stream))

(defmethod write-jimple-statement ((self jimple-reference-value-method) stream)
   "RETURNS the string representation of a jimple-reference-value-method
that is used in the Jimple intermediate language.
This corresponds to the Java signature of the method without modifiers and argument names.
Namely: <return-type> ' ' <method-name> '(' (<arg-type>)* (','<arg-type>))* ')'"
   (declare (type stream stream))
   ;; return-type method-name(
   (write-jimple-statement (jana-type self) stream)
   (write-char #\space stream)
   (write-sequence (quote-name (method-name self)) stream)
   (write-char #\( stream)
   ;; argument list
   (dolist-first-last (java-type (argument-types self))
     (progn
       (write-jimple-statement java-type stream)
       (write-char #\, stream))
     (write-jimple-statement java-type stream))
   ;; )
   (write-char #\) stream))
