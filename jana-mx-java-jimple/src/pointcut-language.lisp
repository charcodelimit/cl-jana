;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java.jimple; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        pointcut-language.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Jul  6 15:46:13 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 22:27:57 2010 (+0100)
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

(defmethod has-modifier-p ((self java-method-declaration) (modifier symbol))
  "RETURNS T if the symbol MODIFIER is found in the method-modifiers list
of the java-method-declaration SELF."
  ;#$self/method-modifiers/modifier-list
  (declare #.*standard-optimize-settings*)
  (find modifier
	(modifier-list (method-modifiers self)) :test #'equal))

(defmethod has-modifier-p ((self java-method-declaration) (modifier-list list))
  "RETURNS T if all the modifier symbols in the list MODIFIER-LIST are found
in the method-modifiers list of the java-method-declaration SELF."
  (reduce #'(lambda (x y) (and x y))
	  (map 'list #'(lambda (modifier) (has-modifier-p self modifier))
	       modifier-list)))

(defmethod has-instruction-type-p ((self jimple-closure) (instruction-type symbol))
  "RETURNS T if the jimple-closure SELF contains an instruction of type INSTRUCTION-TYPE.
Otherwise NIL is returned."
  (let ((instructions (instructions self)))
    (dolist (instruction instructions NIL)
      (when (typep instruction instruction-type)
	(return T)))))