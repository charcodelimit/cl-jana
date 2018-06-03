;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl.zip; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;; FILE: specials.lisp
;;;  LANGUAGE: Common-Lisp
;;;
;;;  DESCRIPTION
;;;
;;;    Special Variable Declarations
;;; 
;;; AUTHORS
;;;    <CHR> Christian Hofmann <c.hofmann@utwente.nl>
;;; MODIFICATIONS
;;;    2009-06-02 <CHR> Initial Version.
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

(in-package :io.zip.checksums)

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

;; CHECKSUM related

(defconstant +adler32-base+ 65521)

(defun crc32-table ()
  (let ((table (make-array 512 :element-type '(unsigned-byte 16))))
    (dotimes (n 256 table)
      (let ((c n))
        (declare (type (unsigned-byte 32) c))
        (dotimes (k 8)
          (if (logbitp 0 c)
              (setf c (logxor #xEDB88320 (ash c -1)))
              (setf c (ash c -1)))
          (setf (aref table (ash n 1)) (ldb (byte 16 16) c)
                (aref table (1+ (ash n 1))) (ldb (byte 16 0) c)))))))

(defvar *crc32-table* (crc32-table))