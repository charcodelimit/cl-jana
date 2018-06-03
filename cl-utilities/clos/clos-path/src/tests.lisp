;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.clos.path; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        tests.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;   Unit Test
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Jul  6 16:18:50 2009 (z)
;;; 
;;; Last-Updated: Thu Jul  9 00:15:27 2009 (z)
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

(in-package :UTIL.CLOS.PATH)

(defclass test-class ()
  ((slot-1
    :INITARG :slot-1)
   (slot-2
    :INITARG :slot-2)
   (slot-3
    :INITARG :slot-3)))

;; Test Plan
;; ---------
;; First -- lexer test
;; Second -- parser test
;; Third -- run-time system test
;; Fourth -- compiler test
;; Fifth -- system test

(define-test tokenizer-test
  (assert-true (equal (tokenize-path-expression "aspect/class-annotations")
                      '("ASPECT" ELEMENTS "CLASS-ANNOTATIONS")))             
  (assert-true (equal (tokenize-path-expression "aspect/class-annotations/name/qualified-name[\"+AspectClass+\"]")
                      '("ASPECT" ELEMENTS "CLASS-ANNOTATIONS" ELEMENTS "NAME" ELEMENTS "QUALIFIED-NAME" PREDICATE-BLOCK-BEGIN "\"+AspectClass+\"" PREDICATE-BLOCK-END)))             
  (assert-true (equal (tokenize-path-expression "aspect/class-annotations/name[qualified-name = +AspectClass+]")
                      '("ASPECT" ELEMENTS "CLASS-ANNOTATIONS" ELEMENTS "NAME" PREDICATE-BLOCK-BEGIN "QUALIFIED-NAME" SAME "+ASPECTCLASS+" PREDICATE-BLOCK-END)))
  (assert-true (equal (tokenize-path-expression "aspect/class-annotations/name/qualified-name[@value = +AspectClass+]")
                      '("ASPECT" ELEMENTS "CLASS-ANNOTATIONS" ELEMENTS "NAME" ELEMENTS "QUALIFIED-NAME" PREDICATE-BLOCK-BEGIN "@VALUE" SAME "+ASPECTCLASS+" PREDICATE-BLOCK-END)))
  (assert-true (equal (tokenize-path-expression "aspect/class-annotations/name/qualified-name[{equal @value +AspectClass+}]")
                      '("ASPECT" ELEMENTS "CLASS-ANNOTATIONS" ELEMENTS "NAME" ELEMENTS "QUALIFIED-NAME" PREDICATE-BLOCK-BEGIN SYMBOLIC-EXPRESSION-BLOCK-BEGIN "equal @value +AspectClass+" SYMBOLIC-EXPRESSION-BLOCK-END PREDICATE-BLOCK-END))))


(define-test parser-test
  (progn
    (assert-true
     (equal
      (parse-expression (tokenize-path-expression "aspect/class-annotations"))
      '(QUERY-AST ("ASPECT") (ELEMENTS "CLASS-ANNOTATIONS"))))
    (assert-true
     (equal
      (parse-expression (tokenize-path-expression "aspect/class-annotations/name/qualified-name[\"+AspectClass+\"]"))
      '(QUERY-AST ("ASPECT") (ELEMENTS "CLASS-ANNOTATIONS") (ELEMENTS "NAME") (ELEMENTS "QUALIFIED-NAME" (PREDICATE "\"+AspectClass+\"")))))
    (assert-true
     (equal
      (parse-expression (tokenize-path-expression "aspect/class-annotations/name[qualified-name = +AspectClass+]"))
      '(QUERY-AST ("ASPECT") (ELEMENTS "CLASS-ANNOTATIONS") (ELEMENTS "NAME" (PREDICATE SAME "QUALIFIED-NAME" "+ASPECTCLASS+")))))
    (assert-true
     (equal
      (parse-expression (tokenize-path-expression "aspect/class-annotations/name/qualified-name[@value = +AspectClass+]"))
      '(QUERY-AST ("ASPECT") (ELEMENTS "CLASS-ANNOTATIONS") (ELEMENTS "NAME") (ELEMENTS "QUALIFIED-NAME" (PREDICATE SAME "@VALUE" "+ASPECTCLASS+")))))
    (assert-true
     (equal
      (parse-expression (tokenize-path-expression "aspect/class-annotations/name/qualified-name[{equal @value +AspectClass+}]"))
      '(QUERY-AST ("ASPECT") (ELEMENTS "CLASS-ANNOTATIONS") (ELEMENTS "NAME") (ELEMENTS "QUALIFIED-NAME" (PREDICATE (SYMBOLIC-EXPRESSION "equal @value +AspectClass+"))))))))
                        
(define-test same-function-test
    (progn
      (assert-true (funcall (eval (same-f '("a"))) "a"))
      (assert-false (funcall (eval (same-f '("a"))) "A"))
      (assert-true (funcall (eval (same-f '(1))) 1))
      (assert-true  (funcall (eval (same-f '(1) :test #'>)) 10))
      (assert-false (funcall (eval (same-f '("a"))) "b"))
      (assert-false (funcall (eval (same-f '(0))) 1))
      (assert-false (funcall (eval (same-f '('a))) 'b))
      (assert-false (funcall (eval (same-f '(1) :test #'>)) -10))))

(define-test slot-access
  (let ((test-object (make-instance 'test-class :slot-1 t :slot-2 '(a b))))
    (assert-true (elt (elements test-object "slot-1" #'atom) 0))
    (assert-true (equalp (elements test-object "slot-2" #'atom) #(a b)))
    (assert-true (equalp (elements test-object "slot-2" #'numberp) #()))))

;(defun simple-test ()
;  (setq *o* (make-instance 'test-class :slot-1 (make-instance 'test-class :slot-2 "E")))
;  (setq *u* (make-instance 'test-class :slot-1 (make-instance 'test-class :slot-2 "U")))
;  (setq *v* "E")
;  (macroexpand-1 '(compile-query "*O*" (ELEMENT "SLOT-1" (SAME "SLOT-2" "*V*"))))
;  (path-query "*O*" (ELEMENT "SLOT-1" (SAME "SLOT-2" "*V*")))
;  (path-query "*U*" (ELEMENT "SLOT-1" (SAME "SLOT-2" "*V*"))))

; (setf (slot-value *o* 'slot-1) (make-array 2))
; (setf (elt (slot-value *o* 'slot-1) 0) (MAKE-INSTANCE 'TEST-CLASS :SLOT-1 #("A" "B" "C" "D" "E" "F") :SLOT-2 "E"))
; (setf (elt (slot-value *o* 'slot-1) 1) (MAKE-INSTANCE 'TEST-CLASS :SLOT-1 #("A" "B" "C" "D" "E" "F") :SLOT-2 "F"))

(define-test compiler-test
  (let* ((test-object-1 (make-instance 'test-class :slot-1 #("A" "B" "C" "D" "E" "F") :slot-2 "E"))
         (test-object-2 (make-instance 'test-class :slot-1 test-object-1))
         (slot-2-value "E"))
    ;; Body - simple expressions
    (assert-true
     (equal
      (compile-query 'test-object-2 '((ELEMENTS "SLOT-1")))
      '(LET ((CURRENT-OBJECT TEST-OBJECT-2)) (SETQ CURRENT-OBJECT (ELEMENT-F CURRENT-OBJECT "SLOT-1" 'NIL)) CURRENT-OBJECT)))
    (assert-true
     (equalp
      (eval #.(compile-query 'test-object-2 '((ELEMENTS "SLOT-1"))))
      (make-array 1 :initial-element test-object-1)))
    ;; Body - complex expressions
    ;;   complex expressions can't be tested by simple comparisons because of generated symbols
    (assert-true
     (equalp
      (eval #.(compile-query 'test-object-2 '((ELEMENTS "SLOT-1" (PREDICATE SAME ":SLOT-2" "SLOT-2-VALUE")))))
      (make-array 1 :initial-element test-object-1)))
    ;; Whole Query - simple expressions
    (assert-true
     (equal
      (compile-path-query (parse-expression (tokenize-path-expression "test-object-2/slot-1")))
      '(LET ((CURRENT-OBJECT TEST-OBJECT-2)) (SETQ CURRENT-OBJECT (ELEMENT-F CURRENT-OBJECT "SLOT-1" 'NIL)) CURRENT-OBJECT)))
    (assert-true
     (equalp
      (eval #.(compile-path-query (parse-expression (tokenize-path-expression "test-object-2/slot-1"))))
      (make-array 1 :initial-element test-object-1)))
    ;; Whole Query - complex expressions
    (assert-true
     ;; the slot-1 value, where the content of slot-2 equals the string "E"
     (equalp (eval #.(compile-path-query (parse-expression (tokenize-path-expression "test-object-2/slot-1[:slot-2=\"E\"]"))))
             (make-array 1 :initial-element test-object-1)))
    (assert-true
     ;; the slot-1 value, where the content of slot-2 equals slot-2-value
     (equalp (eval #.(compile-path-query (parse-expression (tokenize-path-expression "test-object-2/slot-1[:slot-2=slot-2-value]"))))
             (make-array 1 :initial-element test-object-1)))
    (assert-true
     ;; the slot-1 value, where slot-2-value is bound
     (equalp (eval #.(compile-path-query (parse-expression (tokenize-path-expression "test-object-2/slot-1[:slot-2]"))))
             (make-array 1 :initial-element test-object-1)))
    (assert-true
     ;; the slot-2 value, where slot-2 equals the string "E"
     (equalp (eval #.(compile-path-query (parse-expression (tokenize-path-expression "test-object-2/slot-1/slot-2[\"E\"]"))))
             (make-array 1 :initial-element slot-2-value)))
     ;; the 4th element of slot-1
    (assert-true
     (equalp (eval #.(compile-path-query (parse-expression (tokenize-path-expression "test-object-2/slot-1/slot-1[:4]"))))
             slot-2-value))
    (format t "~A" (eval #.(compile-path-query (parse-expression (tokenize-path-expression "test-object-2/slot-1/slot-1[:4]")))))
    (assert-true
     ;; the slot-2 value, whose value equals the string "E"
     (equalp (eval #.(compile-path-query (parse-expression (tokenize-path-expression "test-object-2/slot-1/slot-2[{@value}=\"E\"]"))))
             (make-array 1 :initial-element slot-2-value)))
    ))

      
(define-test reader-macro-test
    (assert-true (equal '#$aspect/class-annotations[qualified-name=+AspectClass+]
                        '(path-query
                          "aspect/class-annotations[qualified-name=+AspectClass+]"))))

;(setq *o* (MAKE-INSTANCE 'TEST-CLASS :SLOT-1 "A" :SLOT-2 "B"))
;#$\*o\*/slot-2{(format t "~A" @value)}
;#$\*o\*{(format t "~A" (slot-value (elt @value 0) 'slot-1))}