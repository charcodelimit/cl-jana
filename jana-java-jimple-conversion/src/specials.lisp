;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple.conversion; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        specials.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;    Special Variable Declarations 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Wed Jun  3 22:46:37 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:25:26 2010 (+0100)
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

(in-package :JANA.JAVA.JIMPLE.CONVERSION)

(defconstant +warn-lossy-conversion+ nil)

(defconstant +jimple-filename-extension+ "jimple")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *jimple-java-modifiers* (make-hash-table))

;; There is not yet support for these keywords in the Jimple Representation used in Soot 2.3.0 !
;; (synthetic "synthetic") (bridge "bridge") (var-args "var-args")))
  (defconstant +modifier-pairs+         
    '((public "public") (private "private") (protected "protected") (final "final") (static "static")
      (abstract "abstract") (strictfp "strictfp") (enum "enum")
      (native "native") (synchronized "synchronized")
      (volatile "volatile") (transient "transient")))

  (mapc (lambda (pair) (setf (gethash (car pair) *jimple-java-modifiers*)
                             (cdr pair)))
        +modifier-pairs+)


  (defvar *jimple-token-table* (make-hash-table :test #'equal))

  (defconstant +token-list+  
    '("abstract" "final" "native" "public" "protected" "private" "static" 
      "synchronized" "transient" "volatile" "strictfp" "enum" "annotation"
      "class" "interface" "void" "boolean" "byte" "short" "char" "int" "long"
      "float" "double" "null_type" "unknown" "extends" "implements"
      "breakpoint" "case" "catch" "cmp" "cmpg" "cmpl" "default" 
      "entermonitor" "exitmonitor" "goto" "if" "instanceof" "interfaceinvoke"
      "lengthof" "lookupswitch" "neg" "new" "newarray" "newmultiarray"
      "nop" "ret" "char" "int" "long" "float" "double" "null_type" "unknown"
      "specialinvoke" "staticinvoke" "tableswitch" "throw" "throws" "virtualinvoke"
      "null" "from" "to" "with" "cls"))

  (mapc 
   (lambda (token-name) 
     (setf (gethash token-name *jimple-token-table*) 
           (concatenate 'string "'" token-name "'")))
   +token-list+)
) ;; (eval-when ...)

#.(declaim (inline quote-name))
(defun quote-name (name-string)
  "RETURNS the quoted method name, if the string NAME-STRING corresponds 
to a token from the JIMPLE grammar of Soot 2.3.0.
Otherwise, the original name-string is returned."
  (declare #.*standard-optimize-settings*)
  (when (and (debug-mode) (gethash name-string *jimple-token-table*))
    (format t "~%Quoting method name: ~A:~A" name-string (gethash name-string *jimple-token-table*)))
  (or (gethash name-string *jimple-token-table*)
      name-string))
