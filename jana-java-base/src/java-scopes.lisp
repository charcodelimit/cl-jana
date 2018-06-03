;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.base; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        scopes.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Jun 29 11:10:27 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:23:55 2010 (+0100)
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

(in-package :JANA.JAVA.BASE)

(defclass java-global-scope (jana-global-lexical-scope)
  ((java-types
    :READER java-types
    :INITARG :java-types
    :TYPE hash-table
    :DOCUMENTATION "maps fully-qualified-name -> type"))
  (:DOCUMENTATION "The lexical scope with global extend of the Java language."))

(defun java-global-scope ()
  "Constructor"
  (make-instance 'java-global-scope
                 :java-types (make-value-weak-synchronous-hashtable :size 9 :test 'equal)))

#.(declaim (inline get-type))
(defmethod get-type ((self java-global-scope) (classname string))
  "RETURNS the type as defined in the static-context of the global scope
for the type or classname CLASSNAME. If no such type exists, NIL is returned."
  (gethash classname
           (slot-value 'java-types self)))