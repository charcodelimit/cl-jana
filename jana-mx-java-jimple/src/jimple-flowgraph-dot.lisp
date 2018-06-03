;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java.jimple; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        jimple-flowgraph-viz.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;  Jimple Flowgraph Vizualization
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Sep 21 10:17:36 2009 (z)
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

;;; functionality should be put into separate files

(defun calculate-subgraphs (graph)
  (let ((visited (make-hash-table))
        (current-subgraph '())
        (subgraph-list '())
        (node-list '())
        (current-node)
        (out-degree 0))
    (push (root-node graph)
          node-list)
    (loop :while (> (length node-list) 0)
          :do
            (setq current-node
                  (pop node-list))
            (unless (gethash current-node visited)
              (setf (gethash current-node visited)
                    t)
              (push current-node
                    current-subgraph)
              ;; if out-degree > 1 count outgoing edges, ignoring exceptional branches
              (if (= (length (edges current-node)) 1)
                  (setq out-degree 1)
                  (progn               
                    (setq out-degree 0)
                    (loop :for edge :in (edges current-node)
                          :do
                          (unless (typep edge 'jimple-exceptional-branch-edge)
                            (incf out-degree)))))
              (loop :for edge :in (edges current-node)
                    :do
                     (push (target edge)
                           node-list))
              (when (or (> out-degree 1)
                        (= out-degree 0))
                (push current-subgraph
                      subgraph-list)
                (setq current-subgraph
                      '()))))
    (push current-subgraph
          subgraph-list)
    subgraph-list))

(defmethod graph-to-dot ((self jimple-intraprocedural-flow-graph) &key (label nil) (fontname "Helvetica") (fontsize "11") (node-shape "box") (graph-attributes '()))
  (let ((dot-graph '())
        (attributes (copy-list graph-attributes))
        (font-attributes '())
        (graph-font-attributes '())
        (sink-node (sink-node self))
        (root-node (root-node self))
        (subgraph-list '())
        (sub-dot-graph '())
        (sub-graph-attributes '()))
    (setq dot-graph
          (cons 's-dot::graph '()))
    ;; font-attributes
    (setq font-attributes    
          (cons `(s-dot::fontname ,fontname)
                ()))
    (push `(s-dot::fontsize ,fontsize)
          font-attributes)
    ;; graph-attributes
    (when label
      (push `(s-dot::label ,label)
            attributes))
    (setq graph-font-attributes    
          (cons '(s-dot::fontname "Times")
                ()))
    (push `(s-dot::fontsize "22")
          graph-font-attributes)
    (unless (assoc 's-dot::rankdir graph-attributes)
      (push '(s-dot::rankdir "TB")
            attributes))
    ;(unless (assoc 's-dot::ordering graph-attributes)
    ;  (push '(s-dot::ordering "out")
    ;        attributes))
    (unless (assoc 's-dot::ranksep graph-attributes)
      (push '(s-dot::ranksep "equally")
            attributes))
    ;(unless (assoc 's-dot::nodesep graph-attributes)
    ;  (push '(s-dot::nodesep ".05")
    ;        attributes))
    ;; append font-attributes
    (push (append attributes
                  (copy-list graph-font-attributes))
          dot-graph)
    ;; add nodes           
    (loop :for node :in (nodes self)
          :do
           (if (or (eql node root-node) (eql node sink-node))
               (push (node-to-dot node :shape node-shape :fillcolor util.colors::+yellow+
                                  :node-attributes (append font-attributes (flow-graph-node-attributes self node)))
                     dot-graph)
               (push (node-to-dot node :shape node-shape
                                  :node-attributes (append font-attributes (flow-graph-node-attributes self node)))
                     dot-graph)))
    ;; add edges
    (loop :for node :in (nodes self)
          :do
           (loop :for edge :in (edges node)
                 :do
                 (push (edge-to-dot edge :edge-attributes font-attributes)
                       dot-graph)))
    ;; add sub-graphs
    (setq subgraph-list
          (calculate-subgraphs self))
    (loop :for current-subgraph :in subgraph-list
          :for level :from 0
          :do
           (setq sub-dot-graph '())
           (loop :for node :in current-subgraph
                 :do
                  (push `(s-dot::node-id ((s-dot::id ,(princ-to-string (id node)))))
                        sub-dot-graph))
           (setq sub-dot-graph
                 (nreverse sub-dot-graph))
           (setq sub-graph-attributes
                 `((s-dot::id ,(princ-to-string level))))
           (push '(s-dot::color "white")
                 sub-graph-attributes)
           (push '(s-dot::clusterrank "local")
                 sub-graph-attributes)
           (when sub-dot-graph
             (push (nconc (list 's-dot::cluster                              
                                sub-graph-attributes)
                          sub-dot-graph)
                   dot-graph)))
    ;; return s-dot graph
    (setq dot-graph
          (nreverse dot-graph))
    ;; (pprint dot-graph t)
    dot-graph))

(defmethod flow-graph-node-attributes ((self flow-graph) (node util.graph:node))
  (let ((instruction)
        (node-label ""))
    (loop :for edge :in (edges node)
          :when (typep edge 'jimple-flow-graph-edge)
          :do
            (setq instruction
                  (instruction edge))
            (when (> (length (branch-target-label instruction)) 0)
              (setq node-label
                    (concatenate 'string node-label (branch-target-label instruction)))))
    `((s-dot::label ,node-label))))


(defmethod flow-graph-node-attributes ((self intraprocedural-data-flow-graph) (node util.graph:node))
  (let ((instruction)
        (node-label ""))
    (loop :for edge :in (edges node)
          :when (typep edge 'jimple-flow-graph-edge)
          :do
            (setq instruction
                  (instruction edge))
            (when (> (length (branch-target-label instruction)) 0)
              (setq node-label
                    (concatenate 'string node-label (branch-target-label instruction) "\\n"))))
    (setq node-label
          (concatenate 'string node-label
                       (with-output-to-string (s)
                                              (write-reaching-definitions node self s))))
    `((s-dot::label ,node-label))))

(defmethod node-to-dot ((self util.graph:node) &key (shape "circle") (fillcolor nil) (node-attributes '()))
  (let ((attributes (copy-list node-attributes)))
    (push `(s-dot::id ,(princ-to-string (id self)))
          attributes)
    (unless (assoc 's-dot::width node-attributes)
      (push `(s-dot::width "0.25")
            attributes))
    (unless (assoc 's-dot::height node-attributes)    
      (push `(s-dot::height "0.25")
            attributes))
    (push `(s-dot::shape ,shape)
          attributes)
    (when fillcolor
      (push '(s-dot::style "filled")
            attributes)
      (push `(s-dot::fillcolor ,fillcolor)
            attributes))    
    `(s-dot::node ,(nreverse attributes))))

(defmethod edge-to-dot ((self edge) &key (label "") (style nil) (edge-attributes '()))
  (let ((attributes (copy-list edge-attributes)))
    (push `(s-dot::label ,(normalize-label label))
          attributes)
    (when style
      (push `(s-dot::style ,style)
            attributes))
    (unless (assoc 's-dot::weight attributes)
      (push `(s-dot::weight "1000.0")
            attributes))
    (push `(s-dot::to ,(princ-to-string (id (target self))))
          attributes)
    (push `(s-dot::from ,(princ-to-string (id (source self))))
          attributes)
    `(s-dot::edge ,(nreverse attributes))))

(defmethod edge-to-dot ((self jimple-flow-graph-edge) &key (style nil) (label nil) (edge-attributes '()))
  (if label
      (call-next-method self :label label :style style :edge-attributes edge-attributes)
      (call-next-method self
                        :label (concatenate 'string 
                                            (princ-to-string (instruction-index (instruction self)))
                                            ": "
                                            (jana.java.jimple.conversion:jimple-statement (instruction self)))
                        :style style
                        :edge-attributes edge-attributes)))

(defmethod edge-to-dot ((self jimple-exceptional-branch-edge) &key (style "dotted") (edge-attributes '()))
  (let ((attributes (copy-list edge-attributes)))
    (if (> (length (edges (source self))) 1)
        (unless (assoc 's-dot::constraint attributes)
          (push '(s-dot::weight "250.0")
                attributes))
        (unless (assoc 's-dot::weight attributes)
          (push '(s-dot::weight "1000.0")
                attributes)))
    (call-next-method self
      :label "catch"
      :style style
      :edge-attributes attributes)))
  
(defmethod edge-to-dot ((self flow-graph-branch-edge) &key (label "") (style nil) (edge-attributes '()))
  (let ((attributes (copy-list edge-attributes)))
    (unless (assoc 's-dot::weight attributes)
      (push '(s-dot::weight "1.0")
          attributes))
    (if style
        (call-next-method self :label label :style style :edge-attributes attributes)
        (call-next-method self :label label :style "dotted" :edge-attributes attributes))))

(defmethod edge-to-dot ((self jump-branch-edge) &key (label nil) (style "dotted") (edge-attributes '()))
  (let ((attributes (copy-list edge-attributes)))
    (when (> (length (edges (source self))) 1)
      (unless (assoc 's-dot::constraint attributes)
        (push '(s-dot::weight "250.0")
              attributes)))
    (if label
        (call-next-method self :label (concatenate 'string "jump: " label) :style style :edge-attributes attributes)
        (call-next-method self :label "jump" :style style :edge-attributes attributes))))

(defmethod edge-to-dot ((self then-branch-edge) &key (label nil) (style "dotted") (edge-attributes '()))
  (if label
      (call-next-method self :label (concatenate 'string "then: " label) :style style :edge-attributes edge-attributes)
      (call-next-method self :label "then" :style style :edge-attributes edge-attributes)))

(defmethod edge-to-dot ((self else-branch-edge) &key (label nil) (style "dotted") (edge-attributes '()))
  (if label
      (call-next-method self :label (concatenate 'string "else: " label) :style style :edge-attributes edge-attributes)
      (call-next-method self :label "else" :style style :edge-attributes edge-attributes)))

(defmethod edge-to-dot ((self switch-branch-edge) &key (label nil) (style "dotted") (edge-attributes '()))
  (if label
      (call-next-method self :label (concatenate 'string "case " (princ-to-string (index self)) ": " label) :style style :edge-attributes edge-attributes)
      (call-next-method self :label (concatenate 'string "case " (princ-to-string (index self))) :style style :edge-attributes edge-attributes)))

(defmethod edge-to-dot ((self default-branch-edge) &key (label nil) (style "dotted") (edge-attributes '()))
  (if label
      (call-next-method self :label (concatenate 'string "default: " label) :style style :edge-attributes edge-attributes)
      (call-next-method self :label "default" :style style :edge-attributes edge-attributes)))