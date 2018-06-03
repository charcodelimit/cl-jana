;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-exception-types.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fr Okt  9 09:36:48 2009 (+0200)
;;; 
;;; Last-Updated: Mi Jan  6 18:23:19 2010 (+0100)
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

(in-package #:JANA.JAVA)

;; VM Error
(defconstant +out-of-memory-error+ "java.lang.OutOfMemoryError")
;; Run-Time Exceptions
(defconstant +array-index-out-of-bounds-exception+ "java.lang.ArrayIndexOutOfBoundsException")
(defconstant +array-store-exception+ "java.lang.ArrayStoreException")
(defconstant +negative-array-size-exception+ "java.lang.NegativeArraySizeException")
(defconstant +null-pointer-exception+ "java.lang.NullPointerException")
(defconstant +class-cast-exception+ "java.lang.ClassCastException")
(defconstant +arithmetic-exception+ "java.lang.ArithmeticException")
(defconstant +illegal-monitor-state-exception+ "java.lang.IllegalMonitorStateException")

(defconstant +java-out-of-memory-error+ 
  (java-type +out-of-memory-error+))
(defconstant +java-array-index-out-of-bounds-exception+ 
  (java-type +array-index-out-of-bounds-exception+))
(defconstant +java-array-store-exception+ 
  (java-type +array-store-exception+))
(defconstant +java-negative-array-size-exception+ 
  (java-type +negative-array-size-exception+))
(defconstant +java-null-pointer-exception+ 
  (java-type +null-pointer-exception+))
(defconstant +java-class-cast-exception+ 
  (java-type +class-cast-exception+))
(defconstant +java-arithmetic-exception+ 
  (java-type +arithmetic-exception+))
(defconstant +java-illegal-monitor-state-exception+ 
  (java-type +illegal-monitor-state-exception+))