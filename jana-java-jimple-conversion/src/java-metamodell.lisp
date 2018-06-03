;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple.conversion; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-metamodell.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Sun Aug 31 23:00:42 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:25:43 2010 (+0100)
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

(defgeneric write-full-java-classifier-signature (classifier-declaration-object output-stream)
  (:DOCUMENTATION "Writes the Java classifier declaration object CLASSIFIER-DECLARATION-OBJECT
to the output stream OUTPUT-STREAM."))

;classifier-type-string The classifier type, i.e. class or interface, is determined by the string classifier-type-string"))

(defmethod write-full-java-signature-for-classifier ((self java-classifier-declaration) classifier-type stream)
    "WRITES the Java class signature of the java-classifier-declaration SELF
including modifiers, extends and implements declarations to the output stream STREAM.
This signature has the form:
<class-modifiers> <classifier-type> ' ' <qualified-name> <extends-relation> <implements-relation>"
    (declare #.*standard-optimize-settings*
             (type string classifier-type)
             (type stream stream))
    (write-jimple-statement (class-modifiers self) stream)
    (write-sequence classifier-type stream)
    (write-char #\space stream)
    (write-sequence (qualified-name self) stream)
    (write-jimple-statement (extends-relation self) stream)
    (write-jimple-statement (implements-relation self) stream))

(defmethod write-jimple-classifier-statement ((self java-classifier-declaration)
                                              classifier-type stream)
  "WRITES the Jimple-IL representation of the java-classifier-declaration SELF
to the output stream STREAM.
This representation has the form:
<class-modifiers> <classifier-type> ' ' <qualified-name> <extends-relation> <implements-relation> '#\newline'
 '{' \(<field-declaration>\)* \(method-declaration\)* '#\newline' '}'"
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-full-java-signature-for-classifier self classifier-type stream)
  (write-char #\newline stream)
  (write-char #\{ stream)
  (dolist (field-declaration (fields self))
    (write-char #\newline stream)
    (write-sequence "    " stream)
    (write-jimple-statement field-declaration stream)
    (write-char #\; stream))
  (write-char #\newline stream)
  (dolist (method-declaration (methods self))
    (write-char #\newline stream)
    (write-jimple-statement method-declaration stream))
  (write-char #\} stream)
  (write-char #\newline stream))

(defmethod write-jimple-statement ((self java-class-declaration) stream)
  "WRITES the Jimple-IL representation of a java-class-declaration to the output stream STREAM.
It has the form: 'class' <java-classifier-declaration>"
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-jimple-classifier-statement self "class" stream))

(defmethod write-jimple-statement ((self java-enum-declaration) stream)
  "WRITES the Jimple-IL representation of a java-enum-declaration to the output stream STREAM.
It has the form: 'class' <java-classifier-declaration>"
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-jimple-classifier-statement self "class" stream))

(defmethod write-jimple-statement ((self java-interface-declaration) stream)
  "WRITES the Jimple-IL representation of a java-interface-declaration to the output stream STREAM.
It has the form: 'interface' <java-classifier-declaration>"
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-jimple-classifier-statement self "interface" stream))

(defmethod write-jimple-statement ((self java-field-declaration) stream)
  "WRITES the Jimple-IL representation of a java-field-declaration to the output stream STREAM.
It has the form: <modifiers> <type> ' ' <field-name>"
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-jimple-statement (field-modifiers self) stream)
  (write-jimple-statement (jana-type self) stream)
  (write-char #\space stream)
  (write-sequence (quote-name (qualified-name (signature self))) stream))

(defmethod write-jimple-statement ((self java-modifiers) stream)
   "WRITES the Jimple-IL representation of java-modifiers to the output stream STREAM.
This representation has the form: \(<modifier> ' '\)* "
   (declare #.*standard-optimize-settings*
            (type stream stream))
   (let ((modifier-jimple-representation))
     (dolist (modifier (modifier-list self))
       (setq modifier-jimple-representation
             (gethash modifier *jimple-java-modifiers*))
       (cond (modifier-jimple-representation
              (write-sequence (car modifier-jimple-representation)
                              stream)
              (write-char #\space stream))
             (t
              (when +WARN-LOSSY-CONVERSION+
                (warn "Can't represent the modifier ~A in the Jimple IL." modifier)))))))

(defmethod write-jimple-statement ((self java-implements) stream)
   "WRITES the Jimple-IL representation of
  the java-implements relation.
This representation has the form: ('implements ')? (<super-types>)*"
   (declare #.*standard-optimize-settings*
            (type stream stream))
   (when (first (implements self))
     (write-sequence " implements" stream)
     (dolist-first-last (super-type-chain (implements self))
       (progn
	 (write-jimple-statement (first super-type-chain) stream)
	 (write-char #\, stream))
       (write-jimple-statement (first super-type-chain) stream)
       :always (write-char #\space stream))))

(defmethod write-jimple-statement ((self java-extends) stream)
   "WRITES the Jimple-IL representation of
  the java-implements relation.
This representation has the form: ('implements ')? (<super-types>)*"
   (declare #.*standard-optimize-settings*
            (type stream stream))
   (when (first (extends self))
     (write-sequence " extends" stream)
     (dolist-first-last (super-type-chain (extends self))
       (progn
	 (write-jimple-statement (first super-type-chain) stream)
	 (write-char #\, stream))
       (write-jimple-statement (first super-type-chain) stream)
       :always (write-char #\space stream))))

(defmethod jimple-method-declaration-statement ((self java-method-declaration) stream)
   "WRITES the Jimple-IL representation of ANY java-method-declaration to the output stream STREAM.
This representation has the form:
(<modifier> ' ')* <return-type> ' ' <name> '(' (<argument-type>)* ')'"
   (declare #.*standard-optimize-settings*
            (type stream stream))
   (write-jimple-statement (method-modifiers self) stream)
   (write-jimple-statement (return-type self) stream)
   (write-char #\space stream)
   (write-sequence (quote-name (qualified-name self)) stream)
   (write-char #\( stream)
   (dolist-first-last (type (argument-types self))
     (progn
       (write-jimple-statement type stream)
       (write-sequence ", " stream))
     (write-jimple-statement type stream))
   (write-char #\) stream)
   (when (thrown-exceptions self)
     (write-sequence " throws " stream)
     (dolist-first-last (exception (thrown-exceptions self))
       (progn
	 (write-jimple-statement exception stream)
	 (write-sequence ", " stream))
       (write-jimple-statement exception stream))))

(defmethod write-jimple-statement ((self java-method-declaration) stream)
  "WRITES the Jimple-IL representation of a
  java-method-declaration to the output stream STREAM.
This representation has the form:
'    ' (<modifier> ' ')* <return-type> ' ' <name> '(' (<argument-type>)* ')' ';' '#\newline'"
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-sequence "    " stream)
  (jimple-method-declaration-statement self stream)
  (write-char #\; stream)
  (write-char #\newline stream))    

(defmethod write-jimple-statement ((self java-method-implementation) stream)
  "WRITES the Jimple-IL representation of a
  java-method-declaration to the output stream STREAM.
This representation has the form:
'    ' <method-declaration-statement> '#\linefeed' '    {'  '#\linefeed' <jimple-body> '    }' '#\linefeed'"
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-sequence "    " stream)
  (jimple-method-declaration-statement self stream)
  (write-char #\newline stream)
  (write-sequence "    {" stream)
  (write-char #\newline stream)
  (write-jimple-statement (body self) stream)
  (write-sequence "    }"  stream)
  (write-char #\newline stream))

(defmethod write-java-method-signature ((self java-method-declaration) stream &key (quote-method-name nil))
  "WRITES the Java method signature of the java-method-declaration SELF
including the return type, but without modifiers and  to the output stream STREAM."
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-jimple-statement (return-type self) stream)
  (write-char #\space stream)
  (format stream "~A.~A"
          (qualified-name (owner-class self))
          (if quote-method-name
              (quote-name (qualified-name self))
              (qualified-name self))
          (map 'list #'jimple-statement (argument-types self)))
  (write-char #\( stream)
  (dolist-first-last (type (argument-types self))
     (progn
       (write-jimple-statement type stream)
       (write-sequence ", " stream))
     (write-jimple-statement type stream))
  (write-char #\) stream))

(defmethod write-full-java-classifier-signature ((self java-class-declaration) stream)
  "WRITES the Java method signature of the java-class-declaration SELF 
to the output stream STREAM."
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-full-java-signature-for-classifier self "class" stream))

(defmethod write-full-java-classifier-signature ((self java-enum-declaration) stream)
  "WRITES the full Java method signature including modifiers
of the java-enum-declaration SELF to the output stream STREAM."
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-full-java-signature-for-classifier self "class" stream))

(defmethod write-full-java-classifier-signature ((self java-interface-declaration) stream)
  "WRITES the full Java method signature including modifiers 
of the java-interface-declaration SELF to the output stream STREAM."
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-full-java-signature-for-classifier self "interface" stream))

(defmethod write-full-java-method-signature ((self java-method-declaration) stream &key (quote-method-name nil))
  "WRITES the full Java method signature including modifiers
of the java-method-declaration SELF to the output stream STREAM."
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-jimple-statement (method-modifiers self) stream)
  (write-java-method-signature self stream :quote-method-name quote-method-name))

(defmethod write-full-java-field-signature ((self java-field-declaration) stream)
  "WRITES the full Java field signature including modifiers
of the java-field-declaration SELF to the output stream STREAM."
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-jimple-statement (field-modifiers self) stream)
  (write-char #\space stream)
  (write-jimple-statement (jana-type self) stream)
  (write-char #\space stream)
  (write-sequence (unqualified-name self) stream))