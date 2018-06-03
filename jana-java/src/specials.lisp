;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        specials.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Wed Jun  3 14:25:56 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:23:03 2010 (+0100)
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
;;;****************************************************************************


(in-package :JANA.JAVA)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defconstant +project-suffix+ "project-")
  (defconstant +source-extension+ ".lisp"))

(defconstant +static-modifier+ 'static)
(defconstant +signature-element-separator+ #\.)

(defvar +file-separator-char+ #\/)

(defconstant +load-java-project-in-package+ cl:*package*
  "The package where the symbols used in java-project files are visible.")

(defconstant +string-class-signature+ "java.lang.String"
  "The signature of string-objects in the Java language.")

(defconstant +class-class-signature+ "java.lang.Class"
  "The signature of objects in the Java language that have type 'class'.")
