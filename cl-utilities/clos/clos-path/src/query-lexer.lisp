;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.clos.path; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        query-lexer.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Jul  6 16:19:48 2009 (z)
;;; 
;;; Last-Updated: Mon Jul  6 23:19:11 2009 (z)
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

#.(declaim (inline escape-char-p))
(defun escape-char-p (c)
  (eql c +escape-char+))

#.(declaim (inline whitespace-p))
(defun whitespace-p (c)
  (or (eql #\space c)
      (eql #\tab c)
      (eql #\Newline c)))

#.(declaim (inline end-of-query-expression-p))
(defun end-of-query-expression-p (c inside-paren)
  "RETURNS T if C terminates a query expression."
  (and (not inside-paren)
       (or (whitespace-p c)
	   (eql #\) c))))

#.(declaim (inline end-of-stream-p))
(defun end-of-stream-p (c)
  "RETURNS T if C is a symbol returned by read-char that
indicates the end of the stream."
  (or (eq nil c)
      (eq 'the-end c)))
  
#.(declaim (inline inside-paren-p))
(defun inside-paren-p (paranthesis-count)
  (> paranthesis-count 0))

;; chr: what type has c ?
;;      why use find
(defun count-parantheses (c paranthesis-count)
  (cond
    ((find c +paranthesis-open+)
     (+ paranthesis-count 1))
    ((find c +paranthesis-close+)
     (- paranthesis-count 1))
    (t paranthesis-count)))

#.(declaim (inline delimiter))
(defun delimiter (string current-char position)
  "RETURNS the delimiter type if  CURRENT-CHAR is a delimiter
in STRING. Otherwise NIL is returned."
  (declare #.*standard-optimize-settings*
           (type string string)
	   (type character current-char)
	   (type fixnum position))
  (let ((next-char #\space))
    (declare (type character current-char next-char)) 
    (when (< (+ position 1) (length string))
      (setq next-char
	    (char string (+ position 1))))
    (cond ((and (eq current-char #\/)
		(eq next-char #\/))
	   'descendants)
	  (t
	   (case current-char
	     (#\/ 'elements)
	     (#\[ 'predicate-block-begin)
	     (#\] 'predicate-block-end)
	     (#\{ 'symbolic-expression-block-begin)
	     (#\} 'symbolic-expression-block-end)
	     (#\@ 'at)
	     (#\= 'same)
	     (#\* 'star)
	     (otherwise nil))))))

(declaim (inline literal))
(defun literal (string first last verbatim)
  "RETURNS the substring of STRING between FIRST and LAST and removes the
escape-character. If STRING starts with #\" or VERBATIM is T, then
 the case of the literal is not changed. Otherwise literals are converted
to upper-case."
  (declare (type string string)
	   (type fixnum first last))
  (cond ((= first last)
	 nil)
	(t
	 (let ((literal (subseq string first last)))
	   (unless verbatim
	     (setq literal 
		   (remove +escape-char+ literal)))
	   (unless (or (eql (char literal 0) #\")
		       verbatim)
	     (setq literal (string-upcase literal)))
	   literal))))

(defun tokenize-path-expression (expr-string)
  "A hand written LR(1) lexer for path-expressions"
  (declare #.*standard-optimize-settings*
           (type string expr-string))
  (let ((last-index 0)
        (token-list ())
        (delimiter nil)
        (verbatim nil)
        (literal nil)
        (current-char #\space)
        (last-char #\space))
    (declare (type string expr-string)
             (type integer last-index)
             (type list token-list)
             (type character current-char))
    ;; iterate over the string
    (loop
     :for index :from 0 :to (- (length expr-string) 1)
     :do
      (setq last-char
            current-char)
      (setq current-char
            (char expr-string index))
      ;; delimiter found
      (when (and (not (escape-char-p last-char))
		 (or
		  (and (not verbatim)
		       (member current-char +delimiters+))
		  (and verbatim
		       (eql current-char +symbolic-expression-block-end+))))
        (setq literal
              (literal expr-string last-index index verbatim))
        ;; delimiter delimited a literal
        (when literal
          ;; add literal
	  (push literal token-list))
        ;; continue with delimiter
        (setq delimiter
              (delimiter expr-string current-char index))
        ;; add delimiter
        (when delimiter
	  (push delimiter token-list))
        (setq last-index
              (+ index 1))
        ;; symbolic expression handling
        (cond ((eq delimiter 'symbolic-expression-block-begin)
               (setq verbatim t))
              ((eq delimiter 'symbolic-expression-block-end)
               (setq verbatim nil))
              ((eq delimiter 'descendants)
               ;; ignore the next character if delimiter is of type
               ;; descendants, i.e. we just hit the first '/' of '//'
               (incf index)))))
    ;; check if we have reached the end of the string
    (when (< last-index (length expr-string))
      ;; not yet - the last character was no delimiter
      (setq literal
            (literal expr-string last-index (length expr-string) verbatim))
      ;; add the last literal
      (when literal
	(push literal token-list)))
    ;; return
    (nreverse token-list)))


