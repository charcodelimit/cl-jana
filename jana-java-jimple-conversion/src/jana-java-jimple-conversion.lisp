;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple.conversion; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        jana-java-jimple-conversion.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;    Methods for the conversion of the Jana-Meta model
;;;    to the Jimple-Intermediate Language Representation.
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Sun Aug 31 23:00:42 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:25:46 2010 (+0100)
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

(declaim (inline jimple-statement))
(defgeneric jimple-statement (self)
  (:DOCUMENTATION "Converts the Jimple intermediate language representation
of SELF to a String."))

(defgeneric write-jimple-statement (self stream)
  (:DOCUMENTATION "Writes the Jimple intermediate language representation
of SELF to STREAM."))

(declaim (inline jimple-local-control-transfer-statement))
(defgeneric jimple-local-control-transfer-statement (self stream)
  (:DOCUMENTATION "Prints the Jimple intermediate language representation
of a local-control-transfer-statement SELF to the stream STREAM."))

;; ---

(defmethod jimple-statement ((self t))
  "Outputs the jimple-statement SELF to a string,
which is RETURNED by the method."
  (with-output-to-string (stream)
			 (write-jimple-statement self stream)))

;; ---

(defun write-to-jimple-file (filename jana-metamodel-element)
  "Writes a jana-metamodel-element JANA-METAMODEL-ELEMENT to the
file designated by the pathname-designator FILENAME."
  (let ((out-p
	 (merge-pathnames (concatenate 'string filename "." +JIMPLE-FILENAME-EXTENSION+))))
    (format t "Writing to ~S" out-p)
    (with-open-file (s out-p :direction :output :if-exists :supersede :external-format :ASCII)
      (write-string (jimple-statement jana-metamodel-element) s)))
  t)

;; (time (format t "" (write-to-jimple-file "ORBUtilSystemException-jana" (load-java-metamodel "test/ORBUtilSystemException.lisp"))))