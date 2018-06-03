;;; -*- Mode: Lisp; Package: io.zip -*-
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;; FILE:               inflate-zlib.lisp
;;; LANGUAGE:           Common-Lisp
;;;
;;; DESCRIPTION
;;;    
;;;    Adds support for files containing data in
;;;    ZLIB compressed data format as described in
;;;    RFC 1950 to inflate.cl from Franz Inc.
;;;
;;;
;;; AUTHORS
;;;    <CHR> Christian Hofmann <c.hofmann@utwente.nl>
;;; MODIFICATIONS
;;;    2009-06-02 <CHR> Initial Version.
;;; BUGS
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

(defclass deflate-header ()
  ((compression-method-and-flags
    :READER compression-method-and-flags
    :INITARG :compression-method-and-flags
    :TYPE octet)
   (flags
    :READER flags
    :INITARG :flags
    :TYPE octet)
   (preset-dictionary-identifier
    :READER preset-dictionary-identifier
    :INITARG :preset-dictionary
    :TYPE (integer 0 #xffffffff))
   (header-length
    :READER header-length
    :INITARG :length
    :TYPE integer
    :DOCUMENTATION "The length of the header in Bytes."))
  (:DOCUMENTATION "The information found in the header of deflate compressed data."))

(defclass deflate-trailer ()
  ((adler32
    :READER adler32
    :INITARG :adler32
    :INITFORM 0
    :TYPE (integer 0 #xffffffff)))
  (:DOCUMENTATION "The information found in the trailing 4 Bytes of a vector containing deflate compressed data."))
  
(defmethod compression-method ((self deflate-header))
  "RETURNS the compression method stored in the lower 4 bits."
  (logand #xf (slot-value self 'compression-method-and-flags)))

(defmethod compression-info ((self deflate-header))
  "RETURNS the compression info stored in the higher 4 bits."
  (ash (logand #xf0 (slot-value self 'compression-method-and-flags)) -4))

(defmethod compression-level ((self deflate-header))
  "RETURNS the compression level used by deflate
to compress the input data."
  (ash (logand #xC0 (slot-value self 'flags)) -6))

(defmethod fcheck-value ((self deflate-header))
  "RETURNS the value used to create a header,
 where CMF and FLG, when viewed as a
16-bit unsigned integer stored in MSB order
(CMF*256 + FLG), is a multiple of 31."
  (logand #x1f (slot-value self 'flags)))

(defmethod header-checksum-matches-p ((self deflate-header))
  "RETURNS the result of comparing the checksum of the header
with the header data."
  (= 0
     (mod (+ (ash (slot-value self 'compression-method-and-flags) 8)
             (slot-value self 'flags))
          #x1f)))

(defmethod preset-dictionary-present-p ((self deflate-header))
  "RETURNS the flag that indicates if the deflate-file
contains a preset dictionary."
  (logbitp 5 (slot-value self 'flags)))

(defun read-deflate-header (input-stream)
  "Reads a deflate header from INPUT-STREAM and RETURNS a new
deflate-header instance."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))  
  (unless
      (and (input-stream-p input-stream)
	   (open-stream-p input-stream)
	   (equal (stream-element-type input-stream) '(unsigned-byte 8)))
    (error "Input-Stream must be an open input-stream with :element-type '(unsigned byte 8) !"))
  (let ((instance (make-instance 'deflate-header))
	(flexi-input-stream (io.streams.flexi:make-flexi-stream input-stream)))
    (declare (type flexi-stream flexi-input-stream)
             (type deflate-header instance))
    ; cmf
    (setf (slot-value instance 'compression-method-and-flags)
	  (read-byte flexi-input-stream))
    ; flg
    (setf (slot-value instance 'flags)
	  (read-byte flexi-input-stream))
    (unless (header-checksum-matches-p instance)
      (warn "The header checksum did not match!"))
    ; preset-dictionary
    (when (preset-dictionary-present-p instance)
      ; dictid
      (setf (slot-value instance 'preset-dictionary-identifier)
            (read-unsigned-int16 flexi-input-stream))
      (error "Unsupported preset-dictionary! ID: ~A" (slot-value instance 'preset-dictionary-identifier)))
    (setf (slot-value instance 'header-length)
	  (file-position input-stream))
    instance))

(defun read-deflate-header-leniently (input-stream)
  "RETURNS a deflate-header instance with
only CMF and FLG set. This function is used to determine
if the file is a valid deflate file."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))  
  (unless
      (and (input-stream-p input-stream)
	   (open-stream-p input-stream)
	   (equal (stream-element-type input-stream) '(unsigned-byte 8)))
    (error "Input-Stream must be an open input-stream with :element-type '(unsigned byte 8) !"))
  (let ((instance (make-instance 'deflate-header))
	(flexi-input-stream (io.streams.flexi:make-flexi-stream input-stream)))
    (declare (type flexi-stream flexi-input-stream)
             (type deflate-header instance))
    ; cmf
    (setf (slot-value instance 'compression-method-and-flags)
	  (read-byte flexi-input-stream))
    ; flg
    (setf (slot-value instance 'flags)
	  (read-byte flexi-input-stream))
    instance))


(declaim (inline read-deflate-checksum))
(defun read-deflate-checksum (input-stream)
  "Reads the Adler32 checksum from
INPUT-STREAM and RETURNS the checksum."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))
  (read-unsigned-int32 input-stream))
  
(defun read-deflate-trailer (input-stream)
    "Reads the trailer of deflate compressed data
from INPUT-STREAM and RETURNS a new deflate-trailer instance."
  (declare #.*standard-optimize-settings*
           (type file-stream input-stream))
  (unless
      (and (input-stream-p input-stream)
	   (open-stream-p input-stream)
	   (equal (stream-element-type input-stream) '(unsigned-byte 8)))
    (error "Input-Stream must be an open input-stream with :element-type '(unsigned byte 8) !"))
  (let ((instance (make-instance 'deflate-trailer)))
    (declare (type deflate-trailer instance))
    (cond
      ((file-position input-stream (- (file-length input-stream) 4))
       ; adler32
       (setf (slot-value instance 'adler32)
	     (read-deflate-checksum input-stream))
       instance)
      (t (error "Can't position input-stream!")))))

(defmethod adler32-matches-p ((self deflate-trailer) checksum-value)
  "TRUE if CHECKSUM-VALUE matches the adler32 checksum stored in the deflate-trailer SELF."
  (= (slot-value self 'adler32) checksum-value))

(defmethod skip-deflate-header-without-reading ((self deflate-header) input-stream)
  "Skips the deflate header in input-stream without producing a deflate-header instance.
If possible, the input-stream is repositioned, and no data is read."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))
  (let ((length (slot-value self 'header-length)))
    (declare (type integer length))
    (unless (file-position input-stream length)
      ; input-stream was not positionable lets read n bytes instead
      (dotimes (i 0 length) (read-byte input-stream)))))

(defun is-valid-deflate-file-p (pathname)
   "Tests if PATHNAME designates a file that stores
deflate compressed data with a matching deflate header checksum."
  (declare #.*standard-optimize-settings*
           (type pathname pathname))
  (let ((deflate-header))    
    (with-open-file (s pathname :direction :input :element-type 'octet)
      (setq deflate-header
            (read-deflate-header-leniently s)))
    (header-checksum-matches-p deflate-header)))

(defun is-supported-deflate-file-p (pathname)
  "Tests if PATHNAME designates a file that stores
deflate compressed data with a matching deflate header checksum,
deflate compression, and no preset dictionary."
  (declare #.*standard-optimize-settings*
           (type pathname pathname))
  (let ((deflate-header))
    (with-open-file (s pathname :direction :input :element-type 'octet)
      (setq deflate-header
            (read-deflate-header-leniently s)))
    (and (= (gzip-constants-deflate-compression +GZIP-CONSTANTS+)
            (compression-method deflate-header))
         (not (preset-dictionary-present-p deflate-header))
         (header-checksum-matches-p deflate-header))))

(defun read-zlib-deflate-file (pathname &key (checksum nil) (on-fail #'warn) (package (package-name cl:*package*)))
  "READS the contents of a file whose contents
were deflate compressed according to RFC 1950.
The keyword :package can be used to control in which package the symbols
read from the file can be found or will be created.
The default value if the keyword :package is not used is
\(package-name cl:*package*\)."
  (declare #.*standard-optimize-settings*
           (type pathname pathname))
    (let ((deflate-header)
          (deflate-trailer (make-instance 'deflate-trailer))
          (decompressed-file-content (make-array 0 :element-type 'octet)))
      (with-open-file (input-stream pathname :direction :input :element-type 'octet)
        (setq deflate-header
              (read-deflate-header input-stream))
        (when (debug-mode)
          (format t "~%Decompressing Input File"))
        (setq decompressed-file-content
              (io.streams.flexi:with-output-to-sequence (out)
                (chipz:decompress nil
                              (chipz:make-dstate 'chipz:deflate)
                              input-stream
                              :buffer-size (input-size gzip-trailer))))
        (setq deflate-trailer
              (make-instance 'deflate-trailer :adler32 (read-deflate-checksum input-stream))))
      (when checksum
        (let ((adler32 (make-instance 'io.zip.checksums:adler32-checksum)))
          (io.zip.checksums:update adler32 decompressed-file-content 0 (length decompressed-file-content))
          (if (adler32-matches-p gzip-trailer (io.zip.checksums:result adler32))
              (format t " O.K.")
              (funcall on-fail " ADLER32 Checksums did not match!"))))
      (setq gzip-trailer nil)
      (when (debug-mode)
        (format t "~%Reading Decompressed Data"))
      (let ((cl:*package* (find-package package)))
        (with-input-from-string (in (io.streams.flexi:octets-to-string decompressed-file-content))
          (read in)))))