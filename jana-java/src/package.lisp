;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-user; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        package.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;    Metamodel for the Java Language.
;;;
;;;    This is a CLOS based implementation of a metamodel
;;;    for Java 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Thu Aug  7 22:57:41 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:23:08 2010 (+0100)
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

(in-package :CL-USER)

;private:
;#:project
;#:java-signature-cache

(defpackage #:CL-JANA-JAVA
  (:USE :COMMON-LISP :JANA.METAMODEL :JANA.BASE :JANA.JAVA.BASE)
  (:NICKNAMES #:JANA.JAVA)
  (:EXPORT ; metamodel
           #:java-signature
           #:java-package
           #:java-modifiers
           #:java-implements
           #:java-extends
           #:java-classifier-declaration
           #:java-class-declaration
           #:java-enum-declaration           
           #:java-interface-declaration
           #:java-annotation-declaration
           #:java-field-declaration           
           #:java-class-variable-declaration
           #:java-instance-variable-declaration
           #:java-method-declaration           
           #:java-method-implementation
           #:java-class-modifiers
           #:java-field-modifiers
           #:java-method-modifiers
           #:java-annotation
           #:java-closure
           #:java-variable-reference           
           #:java-variable-declaration
           #:java-local-variable-declaration
           #:jana-scope
           #:jana-local-scope
           #:jana-global-scope
           #:java-variable
           #:java-class-variable
           #:java-instance-variable
           #:java-local-variable
           ; modifiers
           #:public
           #:private
           #:protected
           #:final
           #:static
           #:abstract
           #:strictfp
           #:enum
           #:annotation
           #:native
           #:abstract
           #:strictfp
           #:synchronized
           #:volatile
           #:transient
           #:enum
           #:synthetic
           #:bridge
           #:var-args
           ; typesystem
           #:java-type
           #:java-reference-type
           #:java-array-type
           #:java-object-reference-type
           #:java-primitive-type
           #:java-null-type
           #:java-void-type           
           #:java-numeric-type
           #:java-floating-point-type
           #:java-integral-type
           #:java-boolean-type
           #:java-char           
           #:java-byte
           #:java-short           
           #:java-int           
           #:java-long
           #:java-float
           #:java-double
           ; typesystem constructors
           #:java-basic-type
           #:void
           #:boolean
           #:float
           #:double
           #:byte
           #:short
           #:int
           #:long
           #:char
           #:java-type-signature
           #:array-type-signature
           ; values
           #:java-value
           #:java-null-value
           #:java-constant-value
           #:java-constant-fp-value
           #:java-constant-float-value
           #:java-constant-double-value
           #:java-constant-exponent-free-double-value
           #:java-constant-integral-value
           #:java-constant-boolean-value
           #:java-constant-byte-value
           #:java-constant-char-value           
           #:java-constant-short-value
           #:java-constant-int-value           
           #:java-constant-long-value
           #:java-constant-string-value
           #:java-constant-class-reference
           ; project           
           ; #:project
           #:java-project
           #:java-user-project
           #:java-rt-project
           #:class-names
           #:aspect-names
           #:classes
           #:load-java-class
           #:load-java-class-silently
           #:load-java-classes
           #:load-java-project
           #:save-java-project
           #:save-java-project-file
           #:save-debug-info-file
           #:write-debug-information
           #:update-transformed-classes
           #:transformation-pathname
           #:with-memoization
           #:identity-transformation
           ; util
           #:with-memoization
           ; constructors
           #:make-field-declaration           
           #:make-java-object-reference-type
           ; specials
           #:+class-class-signature+
           #:+java-out-of-memory-error+
           #:+java-array-index-out-of-bounds-exception+
           #:+java-array-store-exception+
           #:+java-negative-array-size-exception+
           #:+java-null-pointer-exception+
           #:+java-class-cast-exception+
           #:+java-arithmetic-exception+
           #:+java-illegal-monitor-state-exception+
           ; methods
           #:qualified-name
           #:unqualified-name
           #:java-class
           #:super-types
           #:direct-super-type
           #:initialize-subclasses
           #:initialize-implementors
           #:java-classifier-equal
           #:java-method-equal
           #:compressed-filename-extension
           #:uncompressed-filename-extension
           ; readers
           #:parent-package
           #:child-packages
           #:child-classifiers
           #:scope           
           #:extends
           #:annotations
           #:field-modifiers
           #:argument-types
           #:exception-types
           #:return-type
           #:owner-class
           #:annotations
           #:source-file-name
           #:method-modifiers
           #:thrown-exceptions
           #:value
           #:element-value-pairs
           #:out-of-range-value
           #:modifier-list
           #:methods
           #:extends-relation
           #:implements-relation
           #:fields
           #:project-name
           #:project-pathname
           #:project-lib-pathname
           #:analysis-pathname
           #:compilation-pathname
           #:jar-file
           #:final-jar-file
           #:classname-dictionary-pathname
           #:lib-jar-files
           #:transformed-class-names
           #:class-annotations
           #:class-modifiers
           #:nested-classifiers
           #:java-version
           #:aspects
           #:classname-dictionary
           #:dimensions
           #:element-type
           #:implements
           #:classifier-declaration
           #:method-declaration
           #:jana-type
           #:memoization-cache)
  (:DOCUMENTATION
   "This package exports classes for Java programming language elements.
    
    Christian Hofmann 2008 - 2009
    This package is provided under a BSD style license.
    See the source files for details."))