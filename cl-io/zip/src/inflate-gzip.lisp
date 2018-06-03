;;; -*- Mode: Lisp; Package: io.zip -*-
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;; FILE:               inflate-gzip.lisp
;;; LANGUAGE:           Common-Lisp
;;;
;;; DESCRIPTION
;;;    
;;;    Adds support for  GZip files to inflate.cl from Franz Inc.
;;;
;;;
;;; AUTHORS
;;;    <CHR> Christian Hofmann <c.hofmann@utwente.nl>
;;; MODIFICATIONS
;;;    2009-05-27 <CHR> Initial Version.
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

(defclass gzip-header ()
  ((magic
    :READER magic
    :INITFORM (make-array 2 :element-type 'octet :initial-contents (gzip-constants-magic +GZIP-CONSTANTS+))
    :TYPE (vector (octet) *))
   (compression-method
    :READER compression-method
    :INITARG :compression-method
    :INITFORM (gzip-constants-deflate-compression +GZIP-CONSTANTS+)
    :TYPE octet)
   (flags
    :READER flags
    :INITARG :flags
    :INITFORM 0
    :TYPE octet)
   (modification-time
    :READER mtime
    :INITARG :mtime
    :INITFORM (encode-universal-time 0 0 0 1 1 1981)
    :TYPE (integer 0 #xffffffff))
    (extra-flags
     :READER xflags
     :INITARG :xflags
     :TYPE octet)
   (operating-system
     :READER os
     :INITARG :os
     :INITFORM #x3 ; Unix
     :TYPE octet)
   (extra-field-length
    :READER xlen
    :INITARG :xlen
    :INITFORM 0
    :TYPE (integer 0 #xffff))
   (extra-field
    :READER extra-field
    :INITARG :extra-field
    :INITFORM (make-array 0 :element-type 'octet)
    :TYPE (vector (octet) *))
   (original-filename
    :READER fname
    :INITARG :fname
    :INITFORM ""
    :TYPE string)
   (comment
    :READER fcomment
    :INITARG :fcomment
    :INITFORM ""
    :TYPE string)
   (crc16
    :READER crc16
    :INITARG :crc16
    :INITFORM 0
    :TYPE (integer 0 #xffff))
   (header-length
    :READER header-length
    :INITARG :length
    :TYPE integer
    :DOCUMENTATION "The length of the header in Bytes."))
  (:DOCUMENTATION "The information found in the header of a GZip file."))

(defclass gzip-trailer ()
  ((crc32
    :READER crc32
    :INITARG :crc32
    :INITFORM 0
    :TYPE (integer 0 #xffffffff))
   (input-size
    :READER input-size
    :INITARG :input-size
    :INITFORM 0
    :TYPE (integer 0 #xffffffff)))
  (:DOCUMENTATION "The information found in the trailing 8 Bytes of a GZip file."))

  
(defmethod ASCII-text-p ((self gzip-header))
  "RETURNS the flag that indicates if gzip-file
contains ASCII data."
  (logbitp 0 (slot-value self 'flags)))

(defmethod CRC16-header-p ((self gzip-header))
  "RETURNS the flag that indicates if the gzip-file
contains a CRC for the header data."  
  (logbitp 1 (slot-value self 'flags)))

(defmethod extra-fields-p ((self gzip-header))
  "RETURNS the flag that indicates if the gzip-file
contains extra fields."
  (logbitp 2 (slot-value self 'flags)))

(defmethod original-filename-present-p ((self gzip-header))
  "RETURNS the flag that indicates if the gzip-file
contains a string with the original filename."
  (logbitp 3 (slot-value self 'flags)))

(defmethod comment-present-p ((self gzip-header))
  "RETURNS the flag that indicates if the gzip-file
contains a comment string."
  (logbitp 4 (slot-value self 'flags)))

(defun validate-flags (flags)
  "TRUE if the reserved bits in flags
are correctly set to zero in accordance with RFC1952."
  (declare (type octet flags))
  (not (or (logbitp 5 flags)
	   (logbitp 6 flags)
	   (logbitp 7 flags))))

(defmethod valid-reserved-bits-p ((self gzip-header))
  "TRUE if the reserved bits are correctly set to zero."
  (validate-flags (slot-value self 'flags)))

(defun starts-with-magic-number-p (stream)
  "TRUE if the stream starts with the GZip magic number."
  (declare (type stream stream))
  (let ((16bit-buffer (make-array 2 :element-type 'octet)))
    (read-sequence 16bit-buffer stream)
    (compare-vectors 16bit-buffer (gzip-constants-magic +GZIP-CONSTANTS+))))

(defmethod crc32-matches-p ((self gzip-trailer) crc-value)
  "TRUE if CRC-VALUE matches the CRC32 stored in the gzip-trailer SELF."
  (= (slot-value self 'crc32) crc-value))

(defun read-gzip-header (input-stream)
  "Reads a gzip header from INPUT-STREAM and RETURNS a new
gzip-header instance."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))  
  (unless
      (and (input-stream-p input-stream)
	   (open-stream-p input-stream)
	   (equal (stream-element-type input-stream) '(unsigned-byte 8)))
    (error "Input-Stream must be an open input-stream with :element-type '(unsigned byte 8) !"))
  (let ((instance (make-instance 'gzip-header ))
	(flexi-input-stream (io.streams.flexi:make-flexi-stream input-stream)))
    (declare (type io.streams.flexi:flexi-stream flexi-input-stream)
             (type gzip-header instance))
    ; id1, id2
    (unless (starts-with-magic-number-p flexi-input-stream)
      (error "Header does not start with the GZip magic number!"))
    ; cm
    (setf (slot-value instance 'compression-method)
	  (read-byte flexi-input-stream))
    (unless (gzip-constants-deflate-compression +GZIP-CONSTANTS+)
      (error "file compressed with unsupported method"))
    ; flg
    (setf (slot-value instance 'flags)
	  (read-byte flexi-input-stream))
    (unless (valid-reserved-bits-p instance)
      (error "bad flags in header"))
    ; mtime
    (setf (slot-value instance 'modification-time)
	  (read-unsigned-int32 flexi-input-stream))
    ; xflags
    (setf (slot-value instance 'extra-flags)
	  (read-byte flexi-input-stream))
    ; os
    (setf (slot-value instance 'operating-system)
	  (read-byte flexi-input-stream))
    ; extra field
    (when (extra-fields-p instance)
       ; xlen
      (setf (slot-value instance 'extra-field-length)
            (read-unsigned-int16 flexi-input-stream))
      (setf (slot-value instance 'extra-field)
	    (make-array (slot-value instance 'extra-field-length) :element-type 'octet))
      (read-sequence (slot-value instance 'extra-field) flexi-input-stream))
    ; name
    (when (original-filename-present-p instance)
      (setf (slot-value instance 'original-filename)
	    (read-null-terminated-string flexi-input-stream)))
    ; name
    (when (comment-present-p instance)
      (setf (slot-value instance 'comment)
	    (read-null-terminated-string flexi-input-stream)))
    ; crc16 of header
    (when (CRC16-header-p instance)
      (setf (slot-value instance 'crc16)
	    (read-unsigned-int16 flexi-input-stream)))
    (setf (slot-value instance 'header-length)
	  (file-position input-stream))
    instance))

(defun read-gzip-trailer (input-stream)
    "Reads a gzip trailer from INPUT-STREAM and RETURNS a new
gzip-trailer instance."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))
  (unless
      (and (input-stream-p input-stream)
	   (open-stream-p input-stream)
	   (equal (stream-element-type input-stream) '(unsigned-byte 8)))
    (error "Input-Stream must be an open input-stream with :element-type '(unsigned byte 8) !"))
  (let ((instance (make-instance 'gzip-trailer)))
    (declare (type gzip-trailer instance))
    (cond
      ((file-position input-stream (- (file-length input-stream) 8))
       ; crc32
       (setf (slot-value instance 'crc32)
	     (read-unsigned-int32 input-stream))
       ; isize
       (setf (slot-value instance 'input-size)
	     (read-unsigned-int32 input-stream))
       instance)
      (t (error "Can't position input-stream!")))))

(defmethod compressed-data-length ((self gzip-header) input-stream)
  "Returns the length of the compressed data. Requires a stream
INPUT-STREAM that supports file-length."
  (let ((file-length (file-length input-stream)))
    (- file-length (slot-value self 'header-length) 8)))
   
(defmethod skip-gzip-header-without-reading ((self gzip-header) input-stream)
  "Skips the gzip header in input-stream without producing a gzip-header instance.
If possible, the input-stream is repositioned, and no data is read."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))
  (let ((length (slot-value self 'header-length)))
    (declare (type integer length))
    (unless (file-position input-stream length)
      ; input-stream was not positionable lets read n bytes instead
      (dotimes (i 0 length) (read-byte input-stream)))))

(defun starts-with-valid-gzip-header (input-stream)
  "Returns T if the GZip header read from INPUT-STREAM is well formed.
The integrity of the header is not verified, even when the CRC16 is present."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))
  (and (starts-with-magic-number-p input-stream)
       ; we only support deflate compression
       (eql (gzip-constants-deflate-compression +GZIP-CONSTANTS+)  (read-byte input-stream))
       ; check if flags are valid
       (validate-flags (read-byte input-stream))))
  
(defun is-gzip-file-p (pathname)
  "Tests if PATHNAME designates a file with the GZIP magic number in its header."
  (declare #.*standard-optimize-settings*
           (type pathname pathname))
  (with-open-file (s pathname :direction :input :element-type 'octet)
    (starts-with-magic-number-p s)))

(defun is-supported-gzip-file-p (pathname)
  "Tests if PATHNAME designates a file with the GZIP magic number in its header,
 and if the GZipped data is stored using DEFLATE compression."
  (declare #.*standard-optimize-settings*
           (type pathname pathname))
  (with-open-file (s pathname :direction :input :element-type 'octet)
    (starts-with-valid-gzip-header s)))

(defun read-gzip-file-old (pathname &key (checksum nil) (on-fail #'warn) (package (package-name cl:*package*)))
  "READS the contents of a GZIPPED file.
NOTE: The maximum uncompressed file size supported, is 2^32 bytes (4GB)!
CRC checks can be performed by setting the keyword-argument :CHECKSUM to T.
The keyword-argument on-fail can be used to set the type of the condition
that is raised when the checksum failed.
The keyword :package can be used to control in which package the symbols
read from the file can be found or will be created.
The default value if the keyword :package is not used is
\(package-name cl:*package*\)."
  (declare #.*standard-optimize-settings*
           (type pathname pathname))
  (let ((gzip-trailer)
        (decompressed-file-content (make-array 0 :element-type 'octet)))
    (with-open-file (input-stream pathname :direction :input :element-type 'octet)
      (setq gzip-trailer
	    (read-gzip-trailer input-stream))
      (file-position input-stream 0)
      (read-gzip-header input-stream)
      (setq decompressed-file-content
	    (io.streams.flexi:with-output-to-sequence (out :vector-length (input-size gzip-trailer))
	      (inflate input-stream out))))
    (when checksum
      (let ((crc32 (make-instance 'io.zip.checksums:crc32-checksum)))
        (io.zip.checksums:update crc32 decompressed-file-content 0 (length decompressed-file-content))
        (if (crc32-matches-p gzip-trailer (io.zip.checksums:result crc32))
            (format t " O.K.")
            (funcall on-fail " CRC32 Checksums did not match!"))))
    (setq gzip-trailer nil)
    (let ((cl:*package* (find-package package)))
      (with-input-from-string (in (io.streams.flexi:octets-to-string decompressed-file-content))
        (read in)))))

(defun read-gzip-file (pathname &key (checksum nil) (on-fail #'warn) (package (package-name cl:*package*)))
  "READS the contents of a GZIPPED file.
NOTE: The maximum uncompressed file size supported, is 2^32 bytes (4GB)!
CRC checks can be performed by setting the keyword-argument :CHECKSUM to T.
The keyword-argument on-fail can be used to set the type of the condition
that is raised when the checksum failed.
The keyword :package can be used to control in which package the symbols
read from the file can be found or will be created.
The default value if the keyword :package is not used is
\(package-name cl:*package*\)."
  (declare #.*standard-optimize-settings*
           (type pathname pathname))
  (let ((gzip-trailer)
        (decompressed-file-content (make-array 0 :element-type 'octet)))
    (with-open-file (input-stream pathname :direction :input :element-type 'octet)
      (setq gzip-trailer
	    (read-gzip-trailer input-stream))
      (file-position input-stream 0)
      (read-gzip-header input-stream)
      (setq decompressed-file-content
            (chipz:decompress nil
                              (chipz:make-dstate 'chipz:deflate)
                              input-stream
                              :buffer-size (input-size gzip-trailer))))
    (when checksum
      (let ((crc32 (make-instance 'io.zip.checksums:crc32-checksum)))
        (io.zip.checksums:update crc32 decompressed-file-content 0 (length decompressed-file-content))
        (if (crc32-matches-p gzip-trailer (io.zip.checksums:result crc32))
            (format t " O.K.")
            (funcall on-fail " CRC32 Checksums did not match!"))))
    (setq gzip-trailer nil)
    (let ((cl:*package* (find-package package)))
      (with-input-from-string (in (io.streams.flexi:octets-to-string decompressed-file-content))
        (read in)))))


(defun decompress-gzip-file (in-pathname out-pathname)
  "Decompresses a gzip-compressed file IN-PATHNAME
and writes the decompressed data to OUT-PATHNAME.
No CRC checks are performed."
    (declare #.*standard-optimize-settings*
           (type pathname in-pathname out-pathname))  
  (with-open-file (in in-pathname :direction :input :element-type 'octet)
    (read-gzip-trailer in)
    (file-position in 0)
    (read-gzip-header in)
    (with-open-file (out out-pathname :direction :output :if-exists :supersede :element-type 'octet)
      (inflate in out))))