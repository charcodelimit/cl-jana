;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java.jimple; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        cfa.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Tue Sep  8 13:22:09 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 22:27:58 2010 (+0100)
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

(in-package :JANA.MX.JAVA.JIMPLE)

;(use-package util.graph

;; Veraltet!

;(defclass cflow-graph (graph)
;  ((start-node
;    :DOCUMENTATION "The start-node of the control flow graph"))
;  (:DOCUMENTATION "A control flow graph."))
;
;(defmethod initialize-instance :after ((self cflow-graph) &rest initargs &key &allow-other-keys)
;  "Initializes the start node."
;  (setf (slot-value self 'start-node)
;        (add-node self)))
; 
;(defclass cflow-edge (edge)
;  ((instruction
;    :ACCESSOR instruction
;    :TYPE jimple-instruction))
;  (:DOCUMENTATION "A CFLOW edge."))
;
;(defmethod local-cfg (self java-method-implementation)
;  (let ((graph (make-graph :edge-type 'cflow-edge)))
;    (loop :for instruction :in (instructions (body self))
;          :do (add-edge graph (current-node) :instruction instruction))))