;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.clos.path; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        path.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;;    XPath like path-expressions.
;;;
;;;    The goal is to provide Adaptive Programming functionality
;;;    through path-expressions like in XPath
;;;
;;; BUGS
;;;    - strings like in [name="Test"] are upper-cased [NAME="TEST"]
;;;      27-10-08 fixed
;;;    - escape characters are not correctly removed inside verbatim
;;;      literals
;;;       - needs a lexer that replaces all occurrences of '\<char>' against
;;;         '<char>'. Instead, the character '\' is left untouched in the
;;;         literal string, but honored in the lexer
;;;   
;;; TODO
;;;  - document requirements and grammar
;;;  - implement attributes like @type, @value
;;;  - translate attribute names to the proper lambda-list argument names
;;;  - need to deal with cases where symbol-names contain literals
;;;    like *a*. For example, through quoting with curly brackets '{*a*}'
;;;    or the @ sign '@*a*'.
;;;    - Quoting with at is to be prefferred, as curly brackets could be
;;;      better used to denote node visitors
;;;    - Solved: added escape character \*a\*
;;;  - Implement traversals of query results (e.g. []{})
;;;    - Simple traversals can be already implemented with predicates of
;;;      the form: [{(progn (... @value ...) t)}]
;;;    - Example: (let ((aspect *a*))
;;;                 #$aspect/class-annotations[{(format t "~S" @value) t)}])
;;;  - Implement descendants operator '//' and predicates for descendant
;;;    traversals '/[]/'.
;;;  - Implement wildcards or regular expressions for slot-name strings,
;;;    e.g. "aspect/class-*" (very low priority)
;;;  - maybe provide compile-time switch for recursive flattening of collections
;;;  - add accumulative query support, i.e. execute queries in one traversal
;;;    instead of separate traversals, given that the query prefixes match
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Sun Sep 14 22:34:12 CEST 2008
;;; 
;;; Last-Updated: Mi Jan  6 22:44:55 2010 (+0100)
;;;           By: Christian Hofmann
;;;
;;;BUGS
;;;    - strings like in [name="Test"] are upper-cased [NAME="TEST"]
;;;      27-10-08 fixed
;;;    - escape characters are not correctly removed inside verbatim
;;;      literals
;;;       - needs a lexer that replaces all occurrences of '\<char>' against
;;;         '<char>'. Instead, the character '\' is left untouched in the
;;;         literal string, but honored in the lexer
;;;   
;;;TODO
;;; - document requirements and grammar
;;; - implement attributes like @type, @value
;;; - translate attribute names to the proper lambda-list argument names
;;; - need to deal with cases where symbol-names contain literals
;;;   like *a*. For example, through quoting with curly brackets '{*a*}'
;;;   or the @ sign '@*a*'.
;;;   - Quoting with at is to be prefferred, as curly brackets could be
;;;     better used to denote node visitors
;;;   - Solved: added escape character \*a\*
;;; - Implement traversals of query results (e.g. []{})
;;;   - Simple traversals can be already implemented with predicates of
;;;     the form: [{(progn (... @value ...) t)}]
;;;   - Example: (let ((aspect *a*))
;;;                #$aspect/class-annotations[{(format t "~S" @value) t)}])
;;; - Implement descendants operator '//' and predicates for descendant
;;;   traversals '/[]/'.
;;; - Implement wildcards or regular expressions for slot-name strings,
;;;   e.g. "aspect/class-*" (very low priority)
;;; - maybe provide compile-time switch for recursive flattening of collections
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

;<symbol>/<slot-name>//<slot-name>[<symbol|slot-name> <predicate> <symbol>]
;<command>
; Later:
;@slot-name to distinguish symbols from slot-names in []
; --> [@<symbol> <predicate> <symbol>]

;; --------------- USE-CASES --------------------
;; (setq *a* (cl-jana:load-java-class "example.jana.classes.TestAspect"))
;; (let ((aspect *a*)) #$aspect[@value=\*a\*]) => *a*
;; (let ((aspect *a*)) #$aspect[0]) => *a*
;; (let ((aspect *a*)) #$aspect[1]) => #()
;; (let ((aspect *a*)) #$aspect["Foo"]) => #()
;; (let ((aspect *a*)) #$aspect[class-annotations]) => *a*
;;
;; (let ((aspect *a*)) #$aspect/class-annotations/name[qualified-name])
;; => (let ((aspect *a*)) #$aspect/class-annotations/name)
;;
;; (let ((books t)) #$books/isbn[{(subtypep (type-of @value) 'fixnum)}])
;;
;; (let ((aspect *a*)) #$aspect/class-annotations/name/qualified-name[{(progn (format t "~S" @value) t)}])
;; (let ((aspect *a*)) #$aspect/class-annotations/name/qualified-name[{(progn (format t "~S" @type) t)}])
;; (let ((aspect *a*)) #$aspect/class-annotations/name/qualified-name[{(progn (format t "~S" @class) t)}])
;; (let ((aspect *a*)) #$aspect/class-annotations[{(progn (format t "~S" @value) t)}])
;;
;; (let ((aspect *a*)) #$aspect/generalization-relation[{@type}='JAVA-EXTENDS]) => #$aspect/generalization-relation
;; (let ((aspect *a*)) #$aspect/class-annotations/name[qualified-name="example.jana.classes.Aspect"]) => #$aspect/class-annotations/name
;; (let ((aspect *a*)) #$aspect/class-annotations/name[qualified-name="foo"])
;;  => nil

;;; --- READER MACROS ---

(defun |#?-reader| (stream subchar arg)
  "Reads a query expression terminated by space, newline, ) or any
 input that is not a character and transforms it to the predicate
\(> \(length \(path-query \"<Query>\"\)\) 0 \)."
  (declare #.*standard-optimize-settings*
           (type stream stream))
   (list '>
         (list 'length
               (|#$-reader| stream subchar arg))
         0))


(defun |#$-reader| (stream subchar arg)
  "Reads a query expression terminated by space, newline, ) or any
 input that is not a character and transforms it to (path-query \"<Query>\")."
  (declare #.*standard-optimize-settings*
           (ignore subchar arg))
  (let ((paranthesis-count 0)
	(c nil)
	(escaped nil)
	(in-paren-p nil))
    (declare (type paranthesis-count fixnum))
    (list 'path-query
	  (with-output-to-string (s)
	   (loop
	       :do (setq c (read-char stream nil 'the-end))
	       :until (end-of-stream-p c)
	       :do
	        (cond
		  (escaped (write-char c s)
			   (setq escaped nil))
		  ;
		  ((escape-char-p c)
		   (write-char c s)
		   (setq escaped t))
		  ;
		  (t
		   (setq paranthesis-count
			 (count-parantheses c paranthesis-count))
		   (setq in-paren-p
			 (inside-paren-p paranthesis-count))
		   (when (or
			  (end-of-query-expression-p c in-paren-p)
			  (and (not in-paren-p)
			       (eql #\) (peek-char nil stream))))
		     (unless (end-of-query-expression-p c in-paren-p)
		       (write-char c s))
		     (return))
		   (write-char c s))))))))


(set-dispatch-macro-character #\# #\$ #'|#$-reader|)
(set-dispatch-macro-character #\# #\? #'|#?-reader|)


;; chr: never used
;(defmacro result-p (vector)
;  "RETURNS T if the query returned a result"
;  (> 0 (length vector)))

(defun symbolic-expression-node-p (expression)
  "returns T if expression is a symbolic expression of the form:
\(symbolic-expression ...\)"
  (and (listp expression)
       (eq (first expression) 'symbolic-expression)))

(defun predicate-node-p (expression)
  "returns T if expression is a predicate expression of the form:
\(predicate ...\)"
  (and (listp expression)
       (eq (first expression) 'predicate)))

;(defmacro path-query (&body body) (dolist (elt body) (format t "~A;" (princ-to-string elt)))) (path-query aspect/[@qualified-name = {class-name-string}]/[@class-annotations = {+AspectClass+})
;object accessors: @value, @type
;comparators: =

;;; CURRENT SYNTAX
;;aspect/class-annotations/name/qualified-name[class-name-string]
;;<value>/<slot-name>/<slot-name>/<slot-name>[<value>]
;;(path-query aspect (elements class-annotation (same (quote qualified-name) class-name-string)))

(defmacro path-query (path-expression)
  "Compiles the string PATH-EXPRESSION that contains the path-query."
  (declare #.*standard-optimize-settings*
           (type string path-expression))
  (let ((query-expression ())
	(token-list ())
	(compiled-query ()))
    (declare (type list query-expression token-list compiled-query))
    (when +clos-path-debug-mode+
      (format t "~%<DEBUG> [Path Expression] ~S" path-expression))
    (setq token-list
	  (tokenize-path-expression path-expression))
    (when +clos-path-debug-mode+
      (format t "~%<DEBUG> [Tokens] ~S" token-list))
    (setq query-expression
	  (parse-expression token-list))
    (when +clos-path-debug-mode+    
      (format t "~%<DEBUG> [Query AST] ~S" query-expression))
    (setq compiled-query
	  (compile-path-query query-expression))
    (when +clos-path-debug-mode+    
      (format t "~%<DEBUG> [Compiled Query] ~S~%~%" compiled-query))
    compiled-query))

(defun compile-path-query (query-expression)
  "Compiles a path-query expression.
RETURNS the compiled query s-expression."
  (declare #.*standard-optimize-settings*
           (type list query-expression))
  (if (eq (first query-expression) 'query-ast)
      (compile-query
       (compile-query-head (second query-expression))
       (nthcdr 2 query-expression))
      (error "Malformed Query-AST -- query-expression is not of form (query-ast ...): ~%~A" query-expression)))

(defun compile-query-head (query-head)
  "RETURNS the compiled query-head by compiling
the list QUERY-HEAD."
  (declare #.*standard-optimize-settings*
           (type list query-head))
  (let ((compiled-query-head ()))
    (cond
      ; symbolic-expression
      ((symbolic-expression-node-p query-head)
       (setq compiled-query-head
	     (read-from-string (second query-head))))
      ; symbol
      ((stringp (first query-head))
       (setq compiled-query-head
	     (find-symbol (first query-head)))
       (unless compiled-query-head
	 (error "The symbol: ~A is undefined." (first query-head))))
      (t
       (error "Malformed query expression -- first element must be a symbolic-expression or symbol: ~%~A" query-head)))
    (when +clos-path-debug-mode+
      (format t "~%<DEBUG> [Compiled Query-Head] ~S" compiled-query-head))
    compiled-query-head))

;; (macroexpand '#$aspect/class-annotations/name/qualified-name)
(defun compile-query (head-object query-body)
  "Compiles a query expression consisting of the HEAD-OBJECT,
and the query-body QUERY-BODY that queries
the slots of head-object."
  (declare #.*standard-optimize-settings*
           (type list query-body))
  (let ((compiled-query-expression ())
        (first-element)
        (slot-name))
    (declare (type list compiled-query-expression))
    (push 'let compiled-query-expression)
    (push `((current-object ,head-object)) compiled-query-expression)
    (dolist (query-element query-body)
      (setq first-element
            (first query-element))
      (cond
	((eq first-element 'elements)
         (setq slot-name (second query-element))
         (unless (third query-element)
           ;; select all slot-elements
           (push `(setq current-object
		     (element-f current-object
		      ,slot-name
		      'nil))
                 compiled-query-expression))
         (dolist (ast-node (nthcdr 2 query-element))
           (cond 
             ((predicate-node-p ast-node)
              (dolist (predicate-expression
                        (compile-predicate 'current-object
                                           slot-name
                                           (cdr ast-node)))
                (push `(setq current-object ,predicate-expression)
                      compiled-query-expression)))
             ((symbolic-expression-node-p ast-node)
              (push `(setq current-object
                      (element-f current-object
                       ,slot-name
                       'nil))
                 compiled-query-expression)
              (push (compile-symbolic-expression (second ast-node) 'current-object)
                    compiled-query-expression))
             (t
              (error "Invalid Path Expression: ~A. Expecting a Predicate or Symbolic-Expression Block."
                     ast-node)))))
	((listp first-element)
         ;; convert queried-object to vector
         (push `(setq current-object
		     (element-f current-object
		      'nil
		      'nil))
               compiled-query-expression)
	 (cond
           ((predicate-node-p first-element)
            (dolist (predicate-expression
                      (compile-predicate 'current-object
                                         nil
                                         (cdr (first query-element))))
              (push `(setq current-object ,predicate-expression)
                    compiled-query-expression)))
           ((symbolic-expression-node-p first-element)
            (push (compile-symbolic-expression (second first-element) 'current-object)
                  compiled-query-expression))
           (t
            (error "Invalid Path Expression: ~A. Expecting a Predicate or Symbolic-Expression Block."
                   (first query-element)))))))
    (if (> (length query-body) 0)
        (push 'current-object compiled-query-expression)
        ;; convert queried-object to vector
        (push '(make-array 1 :initial-element current-object)
              compiled-query-expression))
    (nreverse compiled-query-expression)))

(defun compile-symbolic-expression-wrapper (symbolic-expression)
  "RETURNS an anonymous function with one argument.
Creates bindings for the special variables @value, @type, or @class
on demand, when they are used in the symbolic expression.
The binding for @value is actually always present, as @value is bound to
the anonymous function's argument, i.e. (lambda (@value) ...)."
  (declare #.*standard-optimize-settings*)
  (let ((compiled-symbolic-expression ())
	(let-form ())
	(bindings ())
	(s-exp (read-from-string symbolic-expression))
	(value-sym (read-from-string "@value")))
    (declare (type list compiled-symbolic-expression let-form bindings))
    ;; create (lambda (x))
    (push 'lambda
	  compiled-symbolic-expression)
    (push (list value-sym)
	  compiled-symbolic-expression)
    ;; create bindings for the let-form
    (cond
      ((search "@type" symbolic-expression)
       (let ((type-sym (read-from-string "@type")))
	 (push `(,type-sym (type-of ,value-sym))
	       bindings)))
      ((search "@class" symbolic-expression)
       (let ((class-sym (read-from-string "@class")))
	 (push `(,class-sym (class-of ,value-sym))
	       bindings))))
    ;; only create a let-form if bindings are neccessary
    (cond
      ((> (length bindings) 0)
       (push 'let let-form)
       (push (nreverse bindings)
	     let-form)
       (push s-exp
	     let-form)
       (push (nreverse let-form)
	     compiled-symbolic-expression))
      (t
       (push s-exp
	     compiled-symbolic-expression)))
    ;; return
    (nreverse compiled-symbolic-expression)))

#.(declaim (inline compiled-symbolic-expression))
(defun compile-symbolic-expression (symbolic-expression current-object-symbol)
  "RETURNS a list containing the function call with the argument CURRENT-OBJECT-SYMBOL.
This function call evaluates the symbolic expression SYMBOLIC-EXPRESSION that is
wrapped to bind the special variables @value, @type, and @class."
  `(funcall ,(compile-symbolic-expression-wrapper symbolic-expression)
            ,current-object-symbol))

#.(declaim (inline abbreviated-predicate-p))
(defun abbreviated-predicate-p (predicate-expression)
  "RETURNS T if the list PREDICATE-EXPRESSION is an
 abbreviated predicate expression.
Abbreviations have the form: [<Number>], [<String>],
or [<slot-name>=<expr>]."
  (declare (type list predicate-expression))
  (let ((exp-length (length predicate-expression))
	(exp-element (first predicate-expression)))
    (declare (type fixnum exp-length))
    (cond
      ((= exp-length 1)
       (and (listp exp-element)
            (eq (first exp-element) 'symbolic-expression)))
      ((> exp-length 1)
       (eq exp-element 'same))
      (t 
       nil))))

(defun compile-predicate (current-object-symbol
			  slot-name-string
			  predicate-expression)
  "Compiles a predicate expression"
  (if (abbreviated-predicate-p predicate-expression)
      (compile-abbreviated-predicate current-object-symbol
				     slot-name-string
				     predicate-expression)
      (compile-primitive-predicate current-object-symbol
                                   slot-name-string
                                   predicate-expression)))

(defun compile-primitive-predicate (current-object-symbol
                                    slot-name-string
                                    predicate-expression)
  "RETURNS a compiled predicate expression."
  (let ((compiled-predicate-expression ())
	(expression-element (first predicate-expression))
        (keyword))
    (declare (type list compiled-predicate-expression))
    (when (stringp expression-element)
      (setq expression-element
	    (read-from-string expression-element)))
    (when (= (length predicate-expression) 1)
      (cond
        ((keywordp expression-element)
         (setq keyword
               (read-from-string (symbol-name expression-element)))
         (cond
           ;; [:<ELEMENT-POSITION>]
           ;; e.g. [:0]
           ((numberp keyword)
            (when slot-name-string
              (push
               `(setq ,current-object-symbol
                 (element-f ,current-object-symbol
                            ,slot-name-string
                            'nil))
               compiled-predicate-expression))
            (push `(if (< ,keyword (length ,current-object-symbol))
                    (elt ,current-object-symbol ,keyword)
                    (make-array 0))
                  compiled-predicate-expression))
           ;; [:<SLOT-NAME-STRING>|:<ACCESSOR-NAME-STRING>]
           ;; e.g. [:class-annotations]
           ((symbolp keyword)
            (let ((arg (gensym)))
              (push `(element-f ,current-object-symbol ,slot-name-string
                      #'(lambda (,arg)
                          (and
                           (slot-exists-p ,arg ',keyword)
                           (slot-boundp ,arg ',keyword))))
                    compiled-predicate-expression)))))
        ;; All other cases
        ;; e.g. [<STRING-LITERAL>]  ["example.jana.classes.aspect"]
        (t
         (push `(element-f ,current-object-symbol
		           ,slot-name-string
		           ,(same-f (list expression-element)))
               compiled-predicate-expression))))
    (nreverse compiled-predicate-expression)))

;; (macroexpand '#$aspect/class-annotations[0])
;; (macroexpand '#$aspect/class-annotations/name/qualified-name["example.jana.classes.Aspect"])
;; (macroexpand '#$aspect/class-annotations/name[qualified-name="example.jana.classes.Aspect"])
(defun compile-abbreviated-predicate (current-object-symbol
				      slot-name-string
				      predicate-expression)
  "RETURNS a compiled predicate expression.
Abbreviated predicates have the literal form: [<Number>], [<String>],
or [<slot-name>=<expr>].
Their AST representation is as follows: \(predicate <Number>\),
\(predicate <String>\) \(predicate same <slot-name> <expr>\)."
  (let ((compiled-predicate-expression ())
	(expression-element (first predicate-expression)))
    (declare (type list compiled-predicate-expression))
    (when (stringp expression-element)
      (setq expression-element
	    (read-from-string expression-element)))
    (if (= (length predicate-expression) 1)
	(cond
          ;; [<SYMBOLIC-EXPRESSION>]
          ((symbolic-expression-node-p expression-element)
           (push
            `(element-f ,current-object-symbol
              ,slot-name-string
              ,(compile-symbolic-expression-wrapper (second expression-element)))
            compiled-predicate-expression))
          (t
           (error "Invalid predicate expression ~A!" predicate-expression)))
        (cond
	  ;; [:<SLOT-NAME-STRING> | <SYMBOLIC-EXPRESSION> '=' <EXPRESSION>]
	  ;; e.g. [:qualified-name="example.jana.classes.aspect"]
	  ;;      [{@type}="java-extends"]
	  ((eq expression-element 'same)
           (when slot-name-string
             (push
              `(setq ,current-object-symbol
                     (element-f ,current-object-symbol
		                ,slot-name-string
		                'nil))
              compiled-predicate-expression))           
	   (dolist (exp (compile-equals-predicate current-object-symbol
						  slot-name-string
						  (cdr predicate-expression)))
	     (push exp compiled-predicate-expression)))
          (t
           (error "Invalid predicate expression ~A!" predicate-expression))))
    (nreverse compiled-predicate-expression)))

(defun compile-equals-predicate (current-object-symbol
				 slot-name-string
				 predicate-expression)
  "RETURNS a compiled predicate expression for abbreviated comparisons.
Abbreviated comparisons have the form:
[:<SLOT-NAME-STRING> | <SYMBOLIC-EXPRESSION> '=' <EXPRESSION>]
e.g. [:qualified-name=\"example.jana.classes.aspect\"], or
[{@type}=\"java-extends\"]."
(let* ((compiled-predicate-expression ())
       (elements nil)
       (select-fn nil)
       (element-fn nil)
       (first-symbol (first predicate-expression))
       (first-symbol-type (type-of first-symbol)))
  (declare (type list compiled-predicate-expression))
  (cond
    ((and (subtypep first-symbol-type 'list)
	  (eq (first first-symbol)
              'symbolic-expression))
     (setq element-fn
	   (compile-symbolic-expression-wrapper (second first-symbol)))
     (setq select-fn
	   `(lambda (x)
	     (let ((y (funcall ,element-fn x)))
	      ,(when +clos-path-debug-mode+ '(format t "~%~S" y))
	      (equal y
		     ,(read-from-string (second predicate-expression))))))
     (setq elements
	   `(select ,select-fn
	            ,current-object-symbol)))
    ;; keyword
    ((and (subtypep first-symbol-type 'string)
          (eq (char first-symbol 0) #\:))
     (setq elements
	   `(element-f ,current-object-symbol
		       ,(subseq first-symbol 1)
		       ,(same-f (list (read-from-string
				       (second predicate-expression)))))))
    ;; all other symbols
    ((subtypep first-symbol-type 'string)
     (setq select-fn
	   `(lambda (x)
             x
             (equal ,(read-from-string first-symbol) ,(read-from-string (second predicate-expression)))))
     (setq elements
           `(select ,select-fn
	            ,current-object-symbol)))
    (t
     (error "Incorrect Use of Equals Predicate: ~A" predicate-expression)))
  (push `(if
	  (> (length ,elements) 0)
	  ,current-object-symbol
	  (make-array 0))
	compiled-predicate-expression)
  (nreverse compiled-predicate-expression)))