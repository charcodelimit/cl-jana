;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.base; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        contexts.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Jun 29 11:09:32 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:23:58 2010 (+0100)
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

(defclass java-project-context (jana-meta-level-context jana-compile-time-context)
  ((java-packages
    :ACCESSOR java-packages
    :TYPE hash-table
    :DOCUMENTATION "maps fully-qualified-name -> java-package")
   (java-subtype-relations
    :ACCESSOR sub-types
    :TYPE hash-table
    :DOCUMENTATION "maps class-names to the class-names of direct sub-classes")
   (java-implements-relations
    :ACCESSOR implementors
    :TYPE hash-table
    :DOCUMENTATION "maps interface class-names to the class-names of implementors."))
  (:DOCUMENTATION "The project context of a Java program consists of packages, and types."))

(defun java-project-context ()
  "Constructor"
    (let ((instance (make-instance 'java-project-context)))
      (setf (java-packages instance)
            (make-value-weak-synchronous-hashtable :size 3 :test 'equal))
      (setf (sub-types instance)
            (make-value-weak-synchronous-hashtable :size 31 :test 'equal))
      (setf (implementors instance)
            (make-value-weak-synchronous-hashtable :size 15 :test 'equal))
      instance))

(defmethod add-direct-subtype ((self java-project-context) qualified-name-superclass qualified-name-subclass)
  "Adds the subclass designated by the qualified-name string QUALIFIED-NAME-SUBCLASS
of the class designated by QUALIFIED-NAME-SUPERCLASS to the list of subtypes
of the java-project-context SELF."
  (declare #.*standard-optimize-settings*
           (type string qualified-name-superclass qualified-name-subclass))
  (push qualified-name-subclass
        (gethash qualified-name-superclass (slot-value self 'java-subtype-relations))))

(defmethod add-direct-implementor((self java-project-context) qualified-name-interface qualified-name-implementor)
    "Adds the implementor designated by the qualified-name string QUALIFIED-NAME-IMPLEMENTOR
of the interface designated by QUALIFIED-NAME-INTERFACE to the list of implementors
of the java-project-context SELF."
    (declare #.*standard-optimize-settings*
             (type string qualified-name-interface qualified-name-implementor))
    (push qualified-name-implementor
          (gethash qualified-name-interface (slot-value self 'java-implements-relations))))