;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: ; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        query-parser.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Jul  6 16:37:24 2009 (z)
;;; 
;;; Last-Updated: Wed Jul  8 23:05:20 2009 (z)
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

;; Tokens: element, predicate-block-begin, predicate-block-end, symbolic-expression-block-begin, symbolic-expression-block-end

(defun symbolic-expression-block-p (list)
  "A symbolic-expression delimited by curly brackets that is evaluated to obtain a 
value that is used in the query expression.
Example: {(car x)}/objects -> (slot-value (car x) 'objects)"
  (and (consp list)
       (eq (first list)
	   'symbolic-expression-block-begin)
       (stringp (second list))
       (eq (third list)
	   'symbolic-expression-block-end)))


;; imaginary tokens: 'predicate 'query-ast 'symbolic-expression
(defun parse-expression (token-list)
  "A hand written stack-based parser for path-expressions.
Rewrites the token stream produced by TOKENIZE-PATH-EXPRESSION into an AST.
RETURNS a list representing the AST."
  (declare #.*standard-optimize-settings*
           (type list token-list))
  (let ((head-list ())
	(final-ast ())
	(current-ast-element ())
	(tmp-element ())
	(ast-element-stack ())
	(lhs nil)
	(is-element-attribute nil))
    (declare (type list head-list final-ast current-ast-element tmp-element ast-element-stack)
             (type boolean is-element-attribute))
    ;; token to identify path expressions
    (push 'query-ast head-list)
    ;; test if first-element is a symbol or a symbolic expression-block
    (cond
      ((stringp (first token-list))
       ;; the variable denoting the queried object
       (push (list (pop token-list))
             head-list))
      ((symbolic-expression-block-p (subseq token-list 0 3)) ; 3 tokens look-ahead
       ;; remove symbolic-expression-block-begin token
       (pop token-list)
       ;; the symbolic expression inside the curly brackets
       (push (list 'symbolic-expression (pop token-list))
             head-list)
       ;; remove symbolic-expression-block-end token
       (pop token-list))
      (t
       (error "The first element in a path-expression must be either a symbol or a symbolic-expression-block. ~% First Token was: ~A" (first token-list))))
  (dolist (token token-list)
    (cond
      ;; attribute (state-variable)
      (is-element-attribute
       (push token current-ast-element)
       (setq tmp-element current-ast-element)
       (setq current-ast-element (pop ast-element-stack))
       ; set ast-element := attribute-list . head
       (push (nreverse tmp-element) current-ast-element)
       (setq is-element-attribute nil))
      ;; elements token ('/')
      ((eq token 'elements)
       ;; add current AST-element to AST
       (when current-ast-element
	 (push (nreverse current-ast-element) final-ast))
       ;; create new AST-element
       (setq current-ast-element ())
       (push 'elements current-ast-element)) ; (elements)
      ;;; predicate block ('[..]')
      ((eq token 'predicate-block-begin)
       ;; create new ast-element
       (push current-ast-element ast-element-stack) ; -> stack
       (setq current-ast-element ()) 
       (push 'predicate current-ast-element)) ; (predicate)
      ((eq token 'predicate-block-end)
       (setq tmp-element current-ast-element) 
       ;; get head from stack
       (setq current-ast-element (pop ast-element-stack)) 
       ;; set ast-element := attribute-list . head
       (push (nreverse tmp-element) current-ast-element)) 
      ;; symbolic-expression block
      ((eq token 'symbolic-expression-block-begin)
       (push current-ast-element ast-element-stack)
       (setq current-ast-element ())
       (push 'symbolic-expression current-ast-element))
      ((eq token 'symbolic-expression-block-end)
       (setq tmp-element current-ast-element)
       (setq current-ast-element (pop ast-element-stack))
       (if current-ast-element
           ;; if there was an ast-element on the stack, the symbolic-expression-block was an attribute
           (push (nreverse tmp-element) current-ast-element)
           ;; otherwise it is an element on its own, and we can directly add it to the final AST
           (push (nreverse tmp-element) final-ast)))
      ;; at expression (modifies state)
      ((eq token 'at)
       (push current-ast-element ast-element-stack)
       (setq current-ast-element ())
       (push 'at current-ast-element)
       (setq is-element-attribute t))
      ;; same expression
      ((eq token 'same)
       (setq lhs (pop current-ast-element))
       (push 'same current-ast-element)
       (push lhs current-ast-element))
      ;; else
      (t
       (push token current-ast-element))))
  ;; there is still a current list
  (when current-ast-element
    (push (nreverse current-ast-element) final-ast))
  ;; some tokens were unbalanced, e.g. missing 'predicate-block-end
  (when (> (length ast-element-stack) 0)
    (error "Parse Error: Unbalanced Token's found ~A" ast-element-stack))
  ;; return
  (nconc (nreverse head-list) (nreverse final-ast))))
