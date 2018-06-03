;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: io-streams; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        io.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Sat May 30 00:36:46 2009 (z)
;;; 
;;; Last-Updated: Mon Jun 22 14:18:05 2009 (z)
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

(in-package :io.streams)

(defun read-unsigned-int16 (input-stream)
  "Read a little endian 16-bit unsigned integer from INPUT-STREAM."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))  
  (read-value (gethash :u16 +number-encodings+) input-stream))

(defun read-unsigned-int32 (input-stream)
  "Read a little endian 32-bit unsigned integer from INPUT-STREAM."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))
  (read-value (gethash :u32 +number-encodings+) input-stream))

(defun read-unsigned-int64 (input-stream)
  "Read a little endian 64-bit unsigned integer from INPUT-STREAM."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))  
  (read-value (gethash :u64 +number-encodings+) input-stream))

(defun read-int16 (input-stream)
  "Read a little endian 16-bit two's complement signed integer from INPUT-STREAM."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))  
  (read-value (gethash :s16 +number-encodings+)  input-stream))

(defun read-int32 (input-stream)
  "Read a little endian 32-bit two's complement signed integer from INPUT-STREAM."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))
  (read-value (gethash :s32 +number-encodings+) input-stream))

(defun read-int64 (input-stream)
  "Read a little endian 64-bit two's complement signed integer from INPUT-STREAM."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))
  (read-value (gethash :s64 +number-encodings+) input-stream))

;; belongs to flexi-streams somehow

(defun read-null-terminated-string (stream &optional (max most-positive-fixnum))
  "Reads a NULL-terminated string from the stream STREAM using
the stream's current external format."
  (declare #.*standard-optimize-settings*
           (type stream stream)
           (type fixnum max))
   (with-output-to-string (out)
    (loop repeat max
          for char = (read-char stream)
          until (char= char #\Null)
          do (write-char char out))))

