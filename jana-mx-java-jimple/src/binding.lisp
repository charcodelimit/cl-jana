;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java.jimple; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        binding.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Wed Jun 24 13:26:43 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 22:27:58 2010 (+0100)
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

(defclass binding-declaration ()
  ((context
    :DOCUMENTATION "The context which provides the bound objects.")
   (binding-statement
    :DOCUMENTATION "variable-declaration-statement ::= literal | signature
literal ::= 'name' | 'signature' | 'this' | 'class'
signature ::= ! literal ")
   (argument-position
    :DOCUMENTATION "The argument to which the object referenced by the binding statement should be bound."))
  (:DOCUMENTATION "A binding-declaration is used to bind a variable from a context to an argument with a
specific position in the argument list of a method."))

(defmethod bind-to-static-method ((method java-method-implementation)
                                  (binding-declarations list))
  "RETURNS a static-method invocation instruction that invokes the java-method METHOD
with the variables declared in the binding-declarations given by the list BINDING-DECLARATIONS."
  )

(defmethod bind-to-virtual-method ((object-binding binding-declaration)
                                   (method java-method-declaration)
                                   (binding-declarations list))
  "RETURNS a static-method invocation instruction that invokes the java-method METHOD
with the variables declared in the binding-declarations given by the list BINDING-DECLARATIONS."
  )
  
    