;;; -*- Mode: Lisp; Package: io.streams -*-
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;; FILE:               bcd.lisp
;;; LANGUAGE:           Common-Lisp
;;;
;;; DESCRIPTION
;;;
;;;
;;;
;;;
;;; AUTHORS
;;;    <CHR> Christian Hofmann <c.hofmann@utwente.nl>
;;; MODIFICATIONS
;;;    2009-05-29 <CHR> Initial Version.
;;; BUGS
;;; LEGAL
;;;    
;;;    Copyright C. Hofmann 2009
;;;    mailto:c.hofmann@utwente.nl
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the \"Software\"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the \"Software\"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;;    
;;;***************************************************************************

(in-package :io.streams)

;;; REWRITE !!!
(declaim (inline integer-to-binary-coded-decimal))
(defun integer-to-binary-coded-decimal (int-value)
  (do ((bcd-value (if (< int-value 0) #xD #xC))
       (int-value (if (< int-value 0) (- int-value) int-value))
       (hex 16 (ash hex 4)))
      ((zerop int-value) bcd-value)
    (multiple-value-bind (q r) (truncate int-value 10)
      (incf bcd-value (* hex r))
      (setf int-value q))))

;;; REWRITE !!!
(declaim (inline binary-coded-decimal-to-integer))
(defun binary-coded-decimal-to-integer (bcd-value)
  (do ((sign   (if (= #xC (mod bcd-value 16)) 1 -1))
       (value  0)
       (bcd-value (truncate bcd-value 16))
       (dix 1 (* 10 dix)))
      ((zerop bcd-value) (* sign value))
    (multiple-value-bind (q r) (truncate bcd-value 16)
      (incf value (* dix r))
      (setf bcd-value q))))