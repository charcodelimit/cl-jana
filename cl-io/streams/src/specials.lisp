;;; -*- Mode: Lisp -*- 
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;; FILE: specials.lisp
;;;
;;; AUTHORS
;;;    <CHR> Christian Hofmann <c.hofmann@utwente.nl>
;;; MODIFICATIONS
;;;    2009-05-29 <CHR> Initial Version.
;;; BUGS
;;;
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

(defvar *standard-optimize-settings*
  '(optimize
    speed
    (safety 0)
    (space 0)
    (debug 1)
    (compilation-speed 0))
  "The standard optimize settings used by most declaration expressions.")

(defvar *fixnum-optimize-settings*
  '(optimize
    speed
    (safety 0)
    (space 0)
    (debug 1)
    (compilation-speed 0)
    #+:lispworks (hcl:fixnum-safety 0))
  "Like *STANDARD-OPTIMIZE-SETTINGS*, but \(on LispWorks) with all
arithmetic being fixnum arithmetic.")


(defun init-number-encodings ()
  "Initialize the +NUMBER-ENCODINGS+ hashtable with all
pre-defined encodings."
  (let ((number-encodings (make-hash-table)))
  (setf (gethash :u16 number-encodings)
        (make-number-encoding :u16))
  (setf (gethash :u32 number-encodings)
        (make-number-encoding :u32))
  (setf (gethash :u64 number-encodings)
	(make-number-encoding :u64))
  (setf (gethash :s16 number-encodings)
        (make-number-encoding :s16))
  (setf (gethash :s32 number-encodings)
        (make-number-encoding :s32))
  (setf (gethash :s64 number-encodings)
        (make-number-encoding :s64))
  (setf (gethash :bcd16 number-encodings)
        (make-number-encoding :bcd16))
  (setf (gethash :bcd32 number-encodings)
        (make-number-encoding :bcd32))
  (setf (gethash :bcd64 number-encodings)
        (make-number-encoding :bcd64))
  (setf (gethash :flt number-encodings)
        (make-number-encoding :flt))
  (setf (gethash :dbl number-encodings)
        (make-number-encoding :dbl))
  (setf (gethash :float number-encodings)
        (gethash :flt number-encodings))
  (setf (gethash :double number-encodings)
        (gethash :dbl number-encodings))
  number-encodings))

(defconstant +number-encodings+ (init-number-encodings)
  "A hash-table that maps number-encoding keywords to number-encoding instances.
Don't change it, it should be a constant !")