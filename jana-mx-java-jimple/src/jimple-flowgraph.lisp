;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java.jimple; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        jimple-flowgraph.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;   Flowgraph Class-Hierarchy
;;;   and Intraprocedural Flowgraph implementation 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Sep 21 10:16:31 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 22:27:57 2010 (+0100)
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

;;; General Flowgraph Framework

(defclass flow-graph (rooted-directed-graph edge-labeled-graph)
  ((edge-type
    :INITFORM 'flow-graph-edge))
  (:DOCUMENTATION "A graph representation of dependencies between statements in a program."))

(defclass flow-graph-edge (directed-edge labeled-edge)
  ((instruction
    :READER instruction
    :INITFORM nil))
  (:DOCUMENTATION "A flow-graph edge."))

(defclass jimple-flow-graph (flow-graph)
  ((sink-node
    :ACCESSOR sink-node
    :DOCUMENTATION "A single node with only incoming edges.")
   (method-invocations
    :ACCESSOR method-invocations
    :TYPE list
    :INITFORM '()
    :DOCUMENTATION "A list of all edges corresponding to method invocations."))
  (:DOCUMENTATION ""))

(defclass jimple-flow-graph-edge (flow-graph-edge)
  ((instruction
    :READER instruction
    :INITARG :instruction
    :INITFORM (jimple-nop-instruction -1)
    :TYPE jimple-instruction))
  (:DOCUMENTATION "A flow-graph-edge labeled with jimple-instructions."))

;;; Intraprocedural Flowgraph Framework

(defclass flow-graph-branch-edge (flow-graph-edge)
  ((index
    :ACCESSOR index
    :TYPE fixnum
    :DOCUMENTATION "Orders branches starting from the same node."))
  (:DOCUMENTATION "An edge corresponding to a branch in the flow-graph."))

(defclass jump-branch-edge (flow-graph-branch-edge)
  ((index
    :INITFORM 0))
  (:DOCUMENTATION "An edge that indicates a jump in the program, e.g. through a goto instruction.."))


(defclass then-branch-edge (flow-graph-branch-edge)
  ((index
    :INITFORM 0))
  (:DOCUMENTATION "An edge that indicates the then branch of an if-instruction"))

(defclass else-branch-edge (flow-graph-branch-edge)
  ((index
    :INITFORM 1))
  (:DOCUMENTATION "An edge that indicates the else branch of an if-instruction"))

(defclass switch-branch-edge (flow-graph-branch-edge)
  ()
  (:DOCUMENTATION "Am edge that indicates a branch of a switch-instruction."))

(defclass default-branch-edge (flow-graph-branch-edge)
  ((index
    :INITFORM (- (ash 1 32) 1)
    :DOCUMENTATION "The default branch edge is always the last edge taken.
Setting the index to 2^32 - 1 allows 2^32 - 2 cases to be used in switch instruction."))
  (:DOCUMENTATION "An edge that indicates the default branch of a switch-instruction"))

(defclass jimple-exceptional-branch-edge (jimple-flow-graph-edge flow-graph-branch-edge)
  ()
  (:DOCUMENTATION "An edge that indicates an exceptional branch of execution."))

;; Graph

(defclass jimple-intraprocedural-flow-graph (jimple-flow-graph)
  ((jimple-closure
    :READER closure
    :INITARG :closure
    :TYPE jimple-closure
    :DOCUMENTATION "The jimple closure to which the flow-graph corresponds.")
   (instruction-edges
    :ACCESSOR instruction-edges
    :INITFORM (jana.java.base::make-value-weak-synchronous-hashtable)
    :TYPE hash-table)
   (edge-type
    :INITFORM 'jimple-flow-graph-edge))
  (:DOCUMENTATION "A flow graph with edges corresponding to jimple-instructions that are part of a jimple-closure."))

(defclass intraprocedural-data-flow-graph (jimple-intraprocedural-flow-graph)
  ((df-values
    :INITFORM (make-hash-table)
    :TYPE hash-table))
  (:DOCUMENTATION "A jimple-intraprocedural-flow-graph that has additional node attributes to allow data-flow calculations."))

;; chr: TODO -- doesn't work yet correctly
;#.(deftableslotaccessor df-values node
;    :DOCUMENTATION "Accessor for the node-attribute df-values.")

(defmethod df-values ((self node))
  (declare #.*standard-optimize-settings*)
  (gethash self (slot-value (graph self) 'df-values)))

(defmethod (setf df-values) (value (self node))
  (declare #.*standard-optimize-settings*)
  (setf (gethash self (slot-value (graph self) 'df-values))
        value))

(defmethod instruction-index ((self jimple-flow-graph-edge))
  "RETURNS the instruction index corresponding to the jimple-flow-graph-edge SELF."
  (declare #.*standard-optimize-settings*)
  (instruction-index (slot-value self 'instruction)))

(defmethod create-intraprocedural-data-flow-graph ((self jimple-intraprocedural-flow-graph))
  (let ((instance (change-class self 'intraprocedural-data-flow-graph))
        (size (length (nodes self))))
    (setf (slot-value instance 'df-values)
          (make-hash-table :size size))
    instance))

(defmethod instruction-edge ((instruction jimple-instruction) (self jimple-intraprocedural-flow-graph))
  "READER Method.
RETURNS the edge form the flow-graph SELF that corresponds to the jimple-instruction INSTRUCTION."
  (declare #.*standard-optimize-settings*)
  (gethash instruction (instruction-edges self)))

(defmethod (setf instruction-edge) (edge (instruction jimple-instruction) (self jimple-intraprocedural-flow-graph))
  "WRITER Method.
Associates the edge EDGE in the flow-graph SELF with the jimple-instruction INSTRUCTION."
  (declare #.*standard-optimize-settings*
           (type jimple-flow-graph-edge edge))
  (setf (gethash instruction (instruction-edges self))
        edge))

(defmethod branch-target-node ((self jimple-intraprocedural-flow-graph) target-label)
  "RETURNS the source-node of the edge that corresponds to the instruction from the jimple-closure
CLOSURE with the label TARGET-LABEL, where edge can be found in the jimple-intraprocedural-flow-graph SELF."
  (declare #.*standard-optimize-settings*)
  (let* ((closure (closure self))
         (instruction-index
          (gethash target-label (branch-target-table closure))))
    (source
     (instruction-edge (nth instruction-index (instructions closure))
                       self))))

(defmethod add-edge ((self jimple-intraprocedural-flow-graph) src
                             &key instruction (tgt nil) (node-type (node-type self)) (edge-type (edge-type self)))
  "The keyword :INSTRUCTION is mandatory."
  (initialize-flowgraph-edge
   self
   (call-next-method self src :tgt tgt :node-type node-type :edge-type edge-type)
   instruction))

(defmethod initialize-flowgraph-edge ((self jimple-flow-graph) (edge flow-graph-edge) instruction)
  "Initializes the flow-graph-edge SELF and returns the initialized edge.
Subclass responsibility."
  (declare (ignore self instruction))
  edge)

(defmethod initialize-flowgraph-edge ((self jimple-flow-graph) (edge jimple-flow-graph-edge) instruction)
  "Initializes the jimple-flow-graph-edge with the instruction instruction."
  ;; link instruction to edge
  (setf (slot-value edge 'instruction)
        instruction)
  ;; link edge to instruction
  (setf (instruction-edge instruction self)
        edge)
  edge)

(defun make-instruction-list-graph (closure)
  "Creates a jimple-intraprocedural-flow-graph that is isomorphic
to the instruction-list of the jimple-closure CLOSURE.
RETURNS the flow-graph, and a list with trap-instructions
that were found in the instruction-list of CLOSURE."
  (declare #.*standard-optimize-settings*  
           (type jimple-closure closure))
  (let ((flow-graph)
        (traps '()))
    (declare (type list traps))
    ;; create flow-graph instance
    (setq flow-graph
          (make-instance 'jimple-intraprocedural-flow-graph :closure closure))
    (setf (util.graph:root-node flow-graph)
          (add-node flow-graph))
    ;; initialize instance
    (loop :for instruction :in (instructions closure)
          :with current-node = (root-node flow-graph)
          :with current-edge
          :do
            (cond
              ((typep instruction 'jimple-imaginary-trap-instruction)
               (push instruction traps))
              (t
               (setq current-edge
                     (add-edge flow-graph current-node :instruction instruction))
               (setq current-node
                     (target current-edge)))))
    ;; correct the order of nodes
    (setf (nodes flow-graph)
          (nreverse (nodes flow-graph)))
    ;; correct the order of traps
    (when (> (length traps) 1)
      (setq traps
            (nreverse traps)))
    (values flow-graph traps)))

#.(declaim (inline add-branch-target-edges))
(defun add-branch-target-edges (flow-graph edge &key (edge-type 'flow-graph-branch-edge))
  "Adds edges for all branch-targets of the jimple-branch-instruction INSTRUCTION from the jimple-closure CLOSURE.
The edges are added to the graph FLOW-GRAPH starting from the target-node of the directed-edge EDGE that corresponds to
the instruction. The edge-type can be specified using the keyword :edge-type. If no value is provided,
edges of type flow-graph-branch-edge are added."
  (declare #.*standard-optimize-settings*
           (type jimple-intraprocedural-flow-graph flow-graph)
           (type jimple-flow-graph-edge edge))
  (let ((target-node (target edge))
        (branch-edge))
    (loop :for label :in (branch-targets (instruction edge))
          :for index :by 1
          :do
           (setq branch-edge
                 (add-edge flow-graph target-node
                           :tgt (branch-target-node flow-graph label)
                           :edge-type edge-type))
           (unless (slot-boundp branch-edge 'index)
             (setf (index branch-edge) index)))))

(defun add-branch-targets-goto (flow-graph edge)
  "Add Jump Target of instruction to flow-graph"
  (add-branch-target-edges flow-graph edge :edge-type 'jump-branch-edge))

(defun add-branch-targets-if (flow-graph edge next-node)
  "Add then and else targets to flow-graph"
  (add-edge flow-graph (target edge)
            :tgt next-node
            :edge-type 'else-branch-edge)
  (add-branch-target-edges flow-graph edge :edge-type 'then-branch-edge))

(defun add-branch-targets-switch (flow-graph edge)
  "Add default and case targets to flow-graph"
  (add-edge flow-graph (target edge)
            :tgt (branch-target-node flow-graph
                                     (default-target-label (instruction edge)))
            :edge-type 'default-branch-edge)
  (add-branch-target-edges flow-graph edge :edge-type 'switch-branch-edge))

(defun add-branch-targets-throw (flow-graph edge project traps)
  "Adds a handler block as branch target to the throw instruction,
if a corresponding trap exists.
Performs a ClassHierarchyAnalysis to determine the traps that apply."
  (let* ((branch-targets (branch-target-table (closure flow-graph)))
         (instruction (instruction edge))
         (instruction-index (instruction-index instruction))          
         (java-classifier))
    ;; connect to handler
    (loop :for trap :in traps
          :when (and (>= instruction-index (gethash (start-label trap) branch-targets))
                     (< instruction-index (gethash (end-label trap) branch-targets)))
          :do
           (setq java-classifier
                 (java-class project (qualified-name (jana-type (thrown-exception instruction)))))
           (when (jana.mx.java:subclass-p java-classifier
                             (qualified-name (handled-exception-type trap))) 
             (add-edge flow-graph (target edge)
                       :instruction trap
                       :tgt (branch-target-node flow-graph (handler-label trap))
                       :edge-type 'jimple-exceptional-branch-edge)))))

(defun add-branch-targets-handled-exception (flow-graph edge traps)
  "Adds the exception handler block as branch targets to methods that may throw an exception.
Because no exception information is available such an edge is added to all method invocation
instructions in the scope of a trap instruction."
  (let* ((branch-targets (branch-target-table (closure flow-graph)))
         (instruction (instruction edge))
         (instruction-index (instruction-index instruction)))
    (loop :for trap :in traps
          :when (and (>= instruction-index (gethash (start-label trap) branch-targets))
                     (< instruction-index (gethash (end-label trap) branch-targets)))
          :do
           (add-edge flow-graph (target edge)
                     :instruction trap                     
                     :tgt (branch-target-node flow-graph (handler-label trap))
                     :edge-type 'jimple-exceptional-branch-edge))))

(defun clean-flow-graph (flow-graph)
  "Determines the nodes in the flow-graph that are
reachable from its root-node.
The list of nodes in the flow-graph is then replaced
against the nodes that were reachable.
RETURNS the modified flow-graph."
  (let ((reachable-nodes (make-hash-table))
        (node-stack '())
        (current-node)
        (target-node))
    (declare (type list reachable-nodes node-stack))
    (setf (gethash (root-node flow-graph) reachable-nodes)
          t)
    (push (root-node flow-graph)
          node-stack)
    (loop :while (> (length node-stack) 0)
          :do
            (setq current-node
                  (pop node-stack))
            (loop :for edge :in (edges current-node)
                  :do
                    (setq target-node
                          (target edge))
                    (unless (gethash target-node reachable-nodes)
                      (push target-node
                            node-stack)
                      (setf (gethash target-node reachable-nodes)
                            t))))
    (format t "~%Reachable nodes: ~A (~A)" (hash-table-count reachable-nodes) (length (nodes flow-graph)))
    (loop :for node :being :the :hash-key :in reachable-nodes
          :collect node :into node-list
          :finally  (setf (nodes flow-graph)
                          node-list))
    flow-graph))


(defmethod add-sink-node ((flow-graph jimple-flow-graph))
  "Adds a sink node to the jimple-flow-graph FLOW-GRAPH."
  (let ((sink-node (add-node-in-order flow-graph)))
    (loop :for node :in (nodes flow-graph)
          :when (and (= (length (edges node)) 0)
                     (not (eq node sink-node)))
          :do (add-edge flow-graph node
                        :tgt sink-node
                        :edge-type 'flow-graph-branch-edge))
    (setf (sink-node flow-graph)
          sink-node)))

(defun make-flow-graph (project closure &key (clean nil))
  "Complexity: (Big-Oh (* 3 (length (instructions closure))))"
  (declare #.*standard-optimize-settings*
           (type jimple-closure closure))
  (let ((flow-graph)
        (traps '()))
    ;; create instruction-list graph
    (multiple-value-setq (flow-graph traps)
      (make-instruction-list-graph closure))
    ;; add intraprocedural control-flow edges
    (loop :for node :in (nodes flow-graph)
          :do
           (loop :for edge :in (edges node)
                 :with instruction
                 :with next-node
                 :do
                  (setq instruction (instruction edge))
                  (setq next-node (target edge))
                  ;; method-invocation on RHS of assignment
                  (when (and (typep instruction 'jimple-abstract-imaginary-assignment-instruction)
                             (typep (assignment-source instruction) 'jimple-method-invocation-instruction))
                    (setq instruction
                          (assignment-source instruction)))
                  ;; remove next-edge of branches and returns
                  (when (and (or (typep instruction 'jimple-branch-instruction)
                                 (typep instruction 'jimple-abstract-return-instruction)
                                 (typep instruction 'jimple-throw-instruction))
                             (> (length (edges next-node)) 0))
                    ;; set new node as the new target
                    (setf (target edge)
                          (add-node flow-graph)))
                  ;; add edges for branch in CF
                  (typecase instruction
                    ;; return -- only needed to add new target node
                    (jimple-return-instruction)
                    ;; goto -- add edge to branch-target
                    (jimple-goto-instruction
                     (add-branch-targets-goto flow-graph edge))
                    ;; if -- add then and else edges
                    (jimple-if-instruction
                     (add-branch-targets-if flow-graph edge next-node))
                    ;; switch -- add edges for all case statements 
                    (jimple-switch-instruction
                     (add-branch-targets-switch flow-graph edge))
                    ;; throw -- add edge to handler 
                    (jimple-throw-instruction
                     (add-branch-targets-throw flow-graph edge project traps))
                    ;; traps -- add edge to method-calls in trap scope that may throw
                    (jimple-method-invocation-instruction
                     (add-branch-targets-handled-exception flow-graph edge traps)
                     (push edge
                           (method-invocations flow-graph)))
                    (t))))
    (add-sink-node flow-graph)
    (setf (method-invocations flow-graph)
          (nreverse (method-invocations flow-graph)))
    (if clean
        (clean-flow-graph flow-graph)
        flow-graph)))

