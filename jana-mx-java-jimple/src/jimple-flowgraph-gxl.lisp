;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java.jimple; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        jimple-flowgraph-gxl.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Wed Sep 23 10:53:23 2009 (z)
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

(defmethod edge->gxl-node ((self jimple-flow-graph-edge) (graph graph)
                               &key (label "") (children nil))
  (let ((edge-label ""))
    (setq edge-label
          (util.graph:normalize-label
           (jana.java.jimple.conversion:jimple-statement (instruction self))))
    (setq edge-label
          (concatenate 'string ":" (string-trim '(#\Space) edge-label)))
    (when (> (length label) 0)
      (setq edge-label
            (concatenate 'string label edge-label)))
    (call-next-method self graph :label edge-label :children children)))


(defmethod edge->gxl-node ((self flow-graph-branch-edge) (graph graph)
                               &key (label ":branch") (children nil))
    (call-next-method self graph :label label :children children))

(defmethod edge->gxl-node ((self then-branch-edge) (graph graph)
                               &key (label ":then") (children nil))
    (call-next-method self graph :label label :children children))

(defmethod edge->gxl-node ((self else-branch-edge) (graph graph)
                               &key (label ":else") (children nil))
    (call-next-method self graph :label label :children children))

(defmethod edge->gxl-node ((self jump-branch-edge) (graph graph)
                               &key (label ":jump") (children nil))
    (call-next-method self graph :label label :children children))

(defmethod edge->gxl-node ((self switch-branch-edge) (graph graph)
                               &key (label "") (children nil))
  (let ((edge-label ""))
    (setq edge-label
          (princ-to-string (index self)))
    (setq edge-label
          (concatenate 'string ":" edge-label))
    (when (> (length label) 0)
      (setq edge-label
            (concatenate 'string label edge-label)))
    (call-next-method self graph :label edge-label :children children)))


(defmethod edge->gxl-node ((self default-branch-edge) (graph graph)
                               &key (label ":default") (children nil))
    (call-next-method self graph :label label :children children))

(defmethod edge->gxl-node ((self jimple-exceptional-branch-edge) (graph graph)
                               &key (label ":exception") (children nil))
  (call-next-method self graph :label label :children children))
