;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple.conversion; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        jimple-cflow-instructions.lisp
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
;;; Last-Updated: Mi Jan  6 18:25:34 2010 (+0100)
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

(defmethod write-jimple-statement ((self jimple-method-invocation-instruction) stream)
  "Writes the string representation of the jimple-method-invocation-instruction SELF
that is used in the Jimple intermediate language to the output-stream STREAM.
This has the form: '<' <receiver-class> ': ' <method-reference> '>(' <method-arguments> ', ' ... ')'"
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-char #\< stream)
  (write-jimple-statement (receiver-class self) stream)
  (write-sequence ": " stream)
  (write-jimple-statement (method-reference self) stream)
  (write-char #\> stream)
  ;; argument list
  (write-char #\( stream)
  (dolist-first-last (argument-value (method-arguments self))		     
    (progn
      (write-jimple-statement argument-value stream)
      (write-sequence ", " stream))
    (write-jimple-statement argument-value stream))
  (write-char #\) stream))

(defmethod write-jimple-statement ((self jimple-invoke-static-instruction) stream)
   "Writes the string representation of the jimple-invoke-static-instruction SELF
that is used in the Jimple intermediate language to the output-stream STREAM.
It has the following form:
'staticinvoke <' <receiver-class> ': ' <method-reference> '>(' <method-arguments> ', ' ... ')'"
   (declare #.*standard-optimize-settings*
            (type stream stream))
   (write-sequence "staticinvoke " stream)
   (call-next-method))

(defmethod write-jimple-statement ((self jimple-dynamic-method-invocation-instruction) stream)
   "Writes the string representation for the jimple-dynamic-method-invocation-instruction SELF
that is used in the Jimple intermediate language to the output-stream STREAM.
This has the following form:
<local-variable> '.<' <receiver-class> ': ' <method-reference> '>(' <method-arguments> ', ' ... ')'"
   (declare #.*standard-optimize-settings*
            (type stream stream))
   (write-jimple-statement (local-variable self) stream)
   (write-char #\. stream)
   (call-next-method))

(defmethod write-jimple-statement ((self jimple-invoke-interface-instruction) stream)
   "Writes the string representation of the jimple-invoke-interface-instruction SELF
that is used in the Jimple intermediate language to the output-stream STREAM.
It has the following form:
'interfaceinvoke ' <local-variable> '.<' <receiver-class> ': ' <method-reference> '>(' <method-arguments> ', ' ... ')'"
   (declare #.*standard-optimize-settings*
            (type stream stream))
   (write-sequence "interfaceinvoke " stream)
   (call-next-method))

(defmethod write-jimple-statement ((self jimple-invoke-special-instruction) stream)
   "Writes the string representation of the jimple-invoke-special-instruction SELF
that is used in the Jimple intermediate language to the output-stream STREAM.
It has the following form:
'specialinvoke ' <local-variable> '.<' <receiver-class> ': ' <method-reference> '>(' <method-arguments> ', ' ... ')'"
   (declare #.*standard-optimize-settings*
            (type stream stream))
   (write-sequence "specialinvoke " stream)
   (call-next-method))

(defmethod write-jimple-statement ((self jimple-invoke-virtual-instruction) stream)
   "Writes the string representation of the jimple-invoke-virtual-instruction SELF
that is used in the Jimple intermediate language to the output-stream STREAM.
It has the following form:
'virtualinvoke ' <local-variable> '.<' <receiver-class> ': ' <method-reference> '>(' <method-arguments> ', ' ... ')'"
   (declare #.*standard-optimize-settings*
            (type stream stream))
   (write-sequence "virtualinvoke " stream)
   (call-next-method))

;;; --- returning of control from method invocations ---

(defmethod write-jimple-statement ((self jimple-return-instruction) stream)
   "Writes the string representation of the jimple-return-instruction SELF
that is used in the Jimple intermediate language to the output-stream STREAM.
It has the following form:
'return ' <java-value>"
   (declare #.*standard-optimize-settings*
            (type stream stream))
   (write-sequence "return " stream)
   (write-jimple-statement (return-value self) stream))

(defmethod write-jimple-statement ((self jimple-return-void-instruction) stream)
   "Writes the representation of the jimple-return-void-instruction SELF
that is used in the Jimple intermediate language to the output-stream STREAM.
It has the following form: 'return'"
   (declare #.*standard-optimize-settings*
            (type stream stream))
   (write-sequence "return" stream))

;;; --- non-local control transfer via exceptions ---

(defmethod write-jimple-statement ((self jimple-throw-instruction) stream)
   "Writes the string representation of the jimple-throw-instruction SELF
that is used in the Jimple intermediate language to the output-stream STREAM.
It has the following form:
'throw ' <java-value>"
   (declare #.*standard-optimize-settings*
            (type stream stream))
   (write-sequence "throw " stream)
   (write-jimple-statement (thrown-exception self) stream))

;;;;; -------------------------------------------

(defmethod jimple-local-control-transfer-statement ((self jimple-imaginary-trap-instruction) stream)
  "Writes the string representation of the jimple-imaginary-trap-instruction SELF
that is used in the Jimple intermediate language to the output-stream STREAM.
It takes as parameters an control-transfer-instruction object, and a hash-table that maps branch targets to label names.
The jimple statement has the following form:
'catch ' <handled-exception-type> ' from ' <start-index> ' to ' <end-index> ' with ' <handler-index>"
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-sequence "catch " stream)
  (write-jimple-statement (handled-exception-type self) stream)
  (write-sequence " from " stream)
  (write-sequence (start-label self) stream)
  (write-sequence " to " stream)
  (write-sequence (end-label self) stream)
  (write-sequence " with " stream)
  (write-sequence (handler-label self) stream))

(defmethod jimple-local-control-transfer-statement ((self jimple-goto-instruction) stream)
  "Writes the Jimple-IL representation of the jimple-goto-instruction SELF
to the output-stream STREAM.
This has the form: 'goto ' <branch-target>"
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-sequence "goto " stream)
  (write-sequence (first (branch-targets self)) stream))

(defmethod jimple-local-control-transfer-statement ((self jimple-if-instruction) stream)
  "RETURNS the Jimple-IL representation of the jimple-if-instruction SELF
to the output-stream STREAM.
This has the form: 'if ' <condition-statement> ' goto ' <branch-target>"
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-sequence "if " stream)
  (write-jimple-statement (condition-statement self) stream)
  (write-sequence " goto " stream)
  (write-sequence (first (branch-targets self)) stream))


(defmethod jimple-local-control-transfer-statement ((self jimple-lookup-switch-instruction)
                                                    stream)
  "Writes the Jimple-IL representation of the jimple-lookup-switch-instruction SELF
to the output-stream STREAM.
This has the form: 'lookupswitch(' <local-variable> ')' CRLF '{' CRLF
\('    case ' <lookup-value> ': goto ' <branch-target-label> ';' CRLF\)*
'    default: goto ' <default-target-label> ';' CRLF '}'"
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-sequence "lookupswitch(" stream)
  (write-jimple-statement (switch-argument-value self) stream)
  (write-char #\) stream)
  (write-char #\newline stream)
  (write-sequence "        {" stream)
  (write-char #\newline stream)
  (loop :for lookup-value :in (lookup-values self)
        :for branch-target :in (branch-targets self)
        :do (progn
              (write-sequence "            case " stream)
              (write-jimple-statement lookup-value stream)
              (write-sequence ": goto " stream)
              (write-sequence branch-target stream)
              (write-char #\; stream)
              (write-char #\newline stream)))
  (write-sequence "            default: goto " stream)
  (write-sequence (default-target-label self) stream)
  (write-char #\; stream)
  (write-char #\newline stream)
  (write-sequence "        }" stream))

(defmethod jimple-local-control-transfer-statement ((self jimple-table-switch-instruction)
						    stream)
  "Writes the Jimple-IL representation of the jimple-table-switch-instruction SELF
to the output-stream STREAM.
This has the form: 'tableswitch(' <local-variable> ')' CRLF '{'
\('    case ' <index> ': goto ' <branch-target-label> ';' CRLF\)*
'    default: goto ' <default-target-label> ';' CRLF '}'"
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (let ((low-index (low-index self))
	(high-index (high-index self)))
    (declare (type fixnum low-index high-index))
    (write-sequence "tableswitch(" stream)
    (write-jimple-statement (switch-argument-value self) stream)
    (write-char #\) stream)
    (write-char #\newline stream)
    (write-sequence "        {" stream)
    (write-char #\newline stream)
    (loop :for index :from low-index :to high-index
          :for branch-target :in (branch-targets self)
          :do (progn
                ;; (dotimes (index (+ (- high-index low-index) 1))
                (write-sequence "            case " stream)
                ;; (princ (+ index low-index) stream)
                (princ index stream)
                (write-sequence ": goto " stream)
                ;; (write-sequence (nth index (branch-targets self))
                (write-sequence branch-target stream)
                (write-char #\; stream)
                (write-char #\newline stream)))
    (write-sequence "            default: goto " stream)
    (write-sequence (default-target-label self) stream)
    (write-char #\; stream)
    (write-char #\newline stream)
    (write-sequence "        }" stream)))

(defmethod write-jimple-statement ((self jimple-local-control-transfer-instruction) stream)
   "Writes the string representation of the jimple-local-control-transfer-instruction SELF
that is used in the Jimple intermediate language to the output-stream STREAM."
   (jimple-local-control-transfer-statement self stream))
