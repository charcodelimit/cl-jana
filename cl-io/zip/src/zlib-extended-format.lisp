;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: io.zip; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;; FILE:                zlib-extended-format.lisp
;;; LANGUAGE:            Common-Lisp
;;;
;;; DESCRIPTION
;;;    This is an implementation for an extended ZLIB format that stores as
;;;    last 32-bit big-endian word the size of the uncompressed data.
;;;    Such an implementation allows to use a native ZLIB interface.
;;;    However, this format is very brittle in the sense, that there is no
;;;    way to identify an extended file without further information in form
;;;    of a header. Extending the header makes it definitively incompatible
;;;    with standard ZLIB deflate.
;;;
;;; AUTHORS
;;;    <CHR> Christian Hofmann <c.hofmann@utwente.nl>
;;;
;;; LEGAL
;;;    
;;;    Copyright C. Hofmann 2009
;;;    mailto:c.hofmann@utwente.nl
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by 
;;; the Free Software Foundation, as clarified by the AllegroServe
;;; prequel found in license-allegroserve.txt.
;;;
;;; This code is distributed in the hope that it will be useful,
;;; but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License is in the file 
;;; license-lgpl.txt that was distributed with this file.
;;; If it is not present, you can access it from
;;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;;; Suite 330, Boston, MA  02111-1307  USA
;;;    
;;;***************************************************************************

(in-package :io.zip)

(defun read-extended-deflate-trailer (input-stream)
    "Reads an extended deflate trailer from INPUT-STREAM and RETURNS a new
extended-deflate-trailer instance."
  (declare #.*standard-optimize-settings*
           (type file-stream input-stream))
  (unless
      (and (input-stream-p input-stream)
	   (open-stream-p input-stream)
	   (equal (stream-element-type input-stream) '(unsigned-byte 8)))
    (error "Input-Stream must be an open input-stream with :element-type '(unsigned byte 8) !"))
  (let ((instance (make-instance 'extended-deflate-trailer)))
    (declare (type extended-deflate-trailer instance))
    (cond
      ((file-position input-stream (- (file-length input-stream) 8))
       ; adler32
       (setf (slot-value instance 'adler32)
	     (read-unsigned-int32 input-stream))
       ; isize
       (setf (slot-value instance 'input-size)
	     (read-value (io.streams:make-number-encoding ':be32) input-stream))
       instance)
      (t (error "Can't position input-stream!")))))

(defclass extended-deflate-trailer (deflate-trailer)
  ((input-size
    :READER input-size
    :INITARG :input-size
    :INITFORM 0
    :TYPE (integer 0 #xffffffff)))
   (:DOCUMENTATION "The information found in the trailing 8 Bytes of a vector containing deflate data and
a trailing 32-bit little-endian integer denoting the input-size."))

#+CLISP (declaim (inline native-read-extended-zlib-deflate-file))
#+CLISP
(defun native-read-extended-zlib-deflate-file (pathname)
  "READS the contents of a file whose contents
were deflate compressed according to RFC 1950,
which ends with a trailing le32 integer denoting
the size of the uncompressed data.
This function uses zlib through the foreign-function-call interface."
  (declare #.*standard-optimize-settings*
           (type pathname pathname))
  (with-open-file (input-stream pathname :direction :input :element-type 'octet)
    (let* ((deflate-trailer (read-extended-deflate-trailer input-stream))
           (data-length (file-length input-stream))
           (input-length (input-size deflate-trailer))
           (compressed-file-content (make-array data-length :element-type 'octet))
           (decompressed-file-content (make-array 0 :element-type 'octet)))
      (file-position input-stream 0)
      (ext:read-byte-sequence compressed-file-content input-stream)
      ;(format t "~%Decompressing Input File")
      (setq decompressed-file-content
            (zlib:uncompress compressed-file-content input-length))
       (setq compressed-file-content nil)
       (with-input-from-string (in (io.streams.flexi:octets-to-string decompressed-file-content))
         (read in)))))

#-CLISP (declaim (inline cl-read-extended-zlib-deflate-file))
#-CLISP
(defun cl-read-extended-zlib-deflate-file (pathname)
  "READS the contents of a file whose contents
were deflate compressed according to RFC 1950,
which ends with a trailing le32 integer denoting
the size of the uncompressed data."
  (declare #.*standard-optimize-settings*
           (type pathname pathname))
  (let ((deflate-header)
        (deflate-trailer)
        (data-length 0)
        (input-length 0)
        (decompressed-file-content (make-array 0 :element-type 'octet)))
    (declare (type integer data-length input-length)
             (type (vector (octet) *) decompressed-file-content))
    (with-open-file (input-stream pathname :direction :input :element-type 'octet)
      (setq deflate-trailer
            (read-extended-deflate-trailer input-stream))
      (file-position input-stream 0)
      (setq deflate-header
            (read-deflate-header input-stream))
      (setq input-length
            (input-size deflate-trailer))
      (when (debug-mode)
        (format t "~%Decompressing Input File"))
      (setq decompressed-file-content
            (io.streams.flexi:with-output-to-sequence (out :vector-length (input-size deflate-trailer))
              (inflate input-stream out))))
    (when (debug-mode)
      (format t "~%Reading Decompressed Data"))
    (with-input-from-string (in (io.streams.flexi:octets-to-string decompressed-file-content))
       (read in))))

(defun read-extended-zlib-deflate-file (pathname)
  "READS the contents of a file whose contents
were deflate compressed according to RFC 1950,
which ends with a trailing le32 integer denoting
the size of the uncompressed data."
  (declare #.*standard-optimize-settings*
           (type pathname pathname))
      #+CLISP (native-read-extended-zlib-deflate-file pathname)
      #-CLISP (cl-read-extended-zlib-deflate-file pathname))
      