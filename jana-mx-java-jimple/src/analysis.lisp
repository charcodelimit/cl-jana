;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java.jimple; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        analysis.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Sep 14 12:06:48 2009 (z)
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

(defstruct definition
  (variable-reference nil :type jimple-reference-value-local)
  (instruction-index 0 :type fixnum)
  (:DOCUMENTATION "A local variable and the index of the instruction where the variable was defined."))

(defun definition-key (definition)
  "The name of the variable-reference is used as the key
 for comparing definitions."
  (variable-name (definition-variable-reference definition)))

(defun same-definition-p (definition value)
  "Local-Variable references are equal if the type,
 and the name of the referenced variable are the same."
  (and (typep definition 'definition)
       (typep value 'definition)
       (equal (variable-name (definition-variable-reference definition))
              (variable-name (definition-variable-reference value)))
       (eq (definition-instruction-index definition)
           (definition-instruction-index value))))
             
;(defun solve-dataflow (flow-graph &key initial-set))

(defmethod write-reaching-definitions ((self node) (graph intraprocedural-data-flow-graph) stream)
  "Writes a string representation of the reaching definitions for node SELF in the intraprocedural-data-flow-graph
GRAPH to the output-stream STREAM."
  (write-char #\{ stream)
  (let ((elements (elements (df-values self))))
    (setq elements
          (sort elements #'string-lessp :key #'(lambda (x) (variable-name (definition-variable-reference x)))))
    (dolist (element elements)
      (write-char #\( stream)
      (write-string (variable-name (definition-variable-reference element)) stream)
      (write-char #\, stream)
      (princ (definition-instruction-index element) stream)
      (write-char #\) stream)))
  (write-char #\} stream))

#.(declaim (inline initialize-root-node))
(defun initialize-root-node (root-node flow-graph &key (variables-may-be-undefined nil))
  "Initializes the root-node.
When the key variables-may-be-undefined is set to T,
all local variable declarations are added to the root-node as uninitialized variable definitions."
  (declare #.*standard-optimize-settings*
           (type jimple-intraprocedural-flow-graph flow-graph)
           (type node root-node))
  (if variables-may-be-undefined
      (let ((current-set (make-bucket-hash-set))
            (current-definition))
        ;; add to entrance of first instruction the references to all local-variables defined by the closure
        (loop :for local-variable-declaration :in (local-variables (closure flow-graph))
              :do
              (setq current-definition
                    (make-definition :variable-reference (jimple-reference-value-from-declaration local-variable-declaration)
                                     :instruction-index -1))
              (add-element current-definition current-set)
              :finally (setf (df-values root-node)
                             current-set)))
      (setf (df-values root-node)
            (make-bucket-hash-set))))

(defun add-flow-effect (instruction effects)
  "Adds the flow-effect of the jimple-instruction INSTRUCTION
to the hash-table EFFECTS that represents the domain of possible effects."
  (declare #.*standard-optimize-settings*
           (type jimple-instruction instruction)
           (type hash-table effects))
  (let ((index (instruction-index instruction)))
    (declare (type fixnum index))
    (when (and (typep instruction 'jimple-abstract-imaginary-assignment-instruction)
               (typep (assignment-target instruction) 'jimple-reference-value-local))
      (setf (gethash index effects)
            (make-definition :variable-reference (assignment-target instruction)
                             :instruction-index index)))))

(defmethod initialize-flowgraph-queue ((flow-graph intraprocedural-data-flow-graph))
  "Initializes the dataflow-value sets of the intraprocedural-data-flow-graph FLOW-GRAPH.
RETURNS the initial worklist."
  (declare #.*standard-optimize-settings*)
  (let ((work-map (make-hash-table))
        (work-queue (make-instance 'util.collections:priority-queue :sort-fun #'<=))
        (definitions (make-hash-table))
        (root-node (root-node flow-graph)))
    (declare (type hash-table work-map definitions)
             (type util.collections:priority-queue work-queue))
    ;; initialize root-node
    (initialize-root-node root-node flow-graph)
    ;; initialize nodes and add to worklist
    (loop :for node :in (nodes flow-graph)
          :do
           (unless (eq node root-node)
             (setf (df-values node)
                   (make-bucket-hash-set)))
           (unless (and (> (in-degree node) 0)
                        (> (out-degree node) 1))
             (unless (typep (first (edges node)) 'flow-graph-branch-edge)
               ;; MAGIC NUMBER!
               ;; maximum out degree in Java programs is the maximum branching degree allowed in a switch statement.
               ;; limited to 255 branches (1 Byte)
               (util.collections:enqueue work-queue node 512)
               (setf (gethash node work-map) t))))
    ;; initialize flow-effect of instructions
    (loop :for instruction :in (instructions (closure flow-graph))
          :do
            (add-flow-effect instruction definitions))
    ;(when (= (length (nodes flow-graph)) 80)
    ;  (error "break"))
    (values definitions work-queue work-map)))


(defmethod initialize-flowgraph ((flow-graph intraprocedural-data-flow-graph))
  "Initializes the dataflow-value sets of the intraprocedural-data-flow-graph FLOW-GRAPH.
RETURNS the initial worklist."
  (declare #.*standard-optimize-settings*)
  (let ((worklist '())
        (definitions (make-hash-table))
        (root-node (root-node flow-graph)))
    (declare (type list worklist)
             (type hash-table definitions))
    ;; initialize root-node
    (initialize-root-node root-node flow-graph)
    ;; initialize nodes and add to worklist
    (loop :for node :in (nodes flow-graph)     
          :do
           (unless (eq node root-node)
             (setf (df-values node)
                   (make-bucket-hash-set)))
           (unless (and (> (in-degree node) 0)
                        (> (out-degree node) 1))
             (unless (typep (first (edges node)) 'flow-graph-branch-edge)
               (push node worklist))))
    (setq worklist
          (nreverse worklist))
    ;; initialize flow-effect of instructions
    (loop :for instruction :in (instructions (closure flow-graph))
          :do
            (add-flow-effect instruction definitions))
    ;(when (= (length (nodes flow-graph)) 80)
    ;  (error "break"))
    (values definitions worklist)))

(defmethod propagate-values (source-node target-node)
  "propagates the dataflow values from source-node to target node.
For the target-node, the size of the set of dataflow values is RETURNED
after propagation."
  (with-accessors ((src-datflow-values df-values))
    source-node
    (with-accessors ((tgt-dataflow-values df-values))
      target-node
      (let ((target-set-size (hashset-count tgt-dataflow-values)))
        (when (> (hashset-count src-datflow-values) 0)
          (if (= target-set-size 0)
              ;; target uninitialized
              (setf tgt-dataflow-values
                    (copy-set src-datflow-values))
              ;; otherwise propagate using set-union
              (setf tgt-dataflow-values
                    (hashset-nunion tgt-dataflow-values src-datflow-values))))
        target-set-size))))

(defun calculate-dataflow-effect (edge target-node definitions)
  (declare #.*standard-optimize-settings*
           (type edge edge)
           (type node target-node)
           (type hash-table definitions))
  (when (typep edge 'jimple-flow-graph-edge)
    (let ((generated-definition
           (gethash (instruction-index edge) definitions)))
      (when generated-definition
        (let ((dataflow-values (df-values target-node)))
          (remove-bucket generated-definition
                         dataflow-values)
          (add-element generated-definition
                       dataflow-values))))))

;(DOLIST (CLASS CLASSES) (DOLIST (METHOD (CL-JANA-JAVA:METHODS CLASS)) (WHEN (AND (TYPEP METHOD 'CL-JANA-JAVA:JAVA-METHOD-IMPLEMENTATION) T) (SETQ METHOD-BODY (CL-JANA-METAMODEL:BODY METHOD)) (FORMAT T "~%Method: ~A" (CL-JANA-METAMODEL:QUALIFIED-NAME METHOD)) (SETQ FLOW-GRAPH (CL-JANA-MX-JAVA-JIMPLE::MAKE-FLOW-GRAPH PROJECT METHOD-BODY)) (CL-JANA-MX-JAVA-JIMPLE::REACHING-DEFINITIONS (CL-JANA-MX-JAVA-JIMPLE::CREATE-INTRAPROCEDURAL-DATA-FLOW-GRAPH FLOW-GRAPH))))) took 327,282 microseconds (0.327282 seconds) to run 
;                    with 2 available CPU cores.
;During that period, 288,018 microseconds (0.288018 seconds) were spent in user mode
;                    4,000 microseconds (0.004000 seconds) were spent in system mode
;18,173 microseconds (0.018173 seconds) was spent in GC.
; 9,362,544 bytes of memory allocated.
; 125 minor page faults, 0 major page faults, 0 swaps.
;

;(DOLIST (CLASS CLASSES) (DOLIST (METHOD (CL-JANA-JAVA:METHODS CLASS)) (WHEN (AND (TYPEP METHOD 'CL-JANA-JAVA:JAVA-METHOD-IMPLEMENTATION) T) (SETQ METHOD-BODY (CL-JANA-METAMODEL:BODY METHOD)) (FORMAT T "~%Method: ~A" (CL-JANA-METAMODEL:QUALIFIED-NAME METHOD)) (SETQ FLOW-GRAPH (CL-JANA-MX-JAVA-JIMPLE::MAKE-FLOW-GRAPH PROJECT METHOD-BODY)) (CL-JANA-MX-JAVA-JIMPLE::REACHING-DEFINITIONS (CL-JANA-MX-JAVA-JIMPLE::CREATE-INTRAPROCEDURAL-DATA-FLOW-GRAPH FLOW-GRAPH))))) took 331,220 microseconds (0.331220 seconds) to run 
;                    with 2 available CPU cores.
;During that period, 296,018 microseconds (0.296018 seconds) were spent in user mode
;                    0 microseconds (0.000000 seconds) were spent in system mode
;17,244 microseconds (0.017244 seconds) was spent in GC.
; 7,777,296 bytes of memory allocated.
; 285 minor page faults, 0 major page faults, 0 swaps.
;NIL

;;; chr: ToDo -- modularize!
(defun reaching-definitions (flow-graph)
  (let ((worklist '())
        (node-count (length (nodes flow-graph)))
        (iteration-count 0)
        (current-target)
        (current-df-set-size 0)
        (definitions-table (make-hash-table)))
    (declare (type list worklist)
             (type hash-table definitions-table)
             (type fixnum iteration-count node-count current-df-set-size))
    (multiple-value-setq (definitions-table worklist)
      (initialize-flowgraph flow-graph))
    ;; process worklist
    (loop :while (> (length worklist) 0)
          :with node
          :do
            (setq node
                  (pop worklist))
            (incf iteration-count)
            (loop :for edge :in (edges node)
                  :do
                   (setq current-target
                         (target edge))
                   (setq current-df-set-size                     
                         (propagate-values node current-target))
                   ;; calculate dataflow effect
                   (calculate-dataflow-effect edge current-target definitions-table)
;                   (format t "~% Reaching definitions node ~A: ~A ~A"
;                           (id current-target)
;                           (if (instruction edge)
;                               (jana.java.jimple.conversion:jimple-statement (instruction edge))
;                               "branch")                          
;                           (with-output-to-string (s)
;                             (write-reaching-definitions current-target flow-graph s)))
                   ;; done -- process next element
                   (unless (or (= (hashset-count (df-values current-target))
                                  current-df-set-size)
                               (find current-target worklist))
                     (push current-target
                           worklist))))
    (format t "~% RDA [r:~f p:~A/~A]." (/ iteration-count node-count) iteration-count node-count)
    (/ iteration-count node-count)))


;;; chr: ToDo -- modularize!
(defun reaching-definitions-queue (flow-graph)
  (let ((work-queue (make-instance 'util.collections:priority-queue :sort-fun #'<=))
        (work-table (make-hash-table))
        (node-count (length (nodes flow-graph)))
        (iteration-count 0)
        (extra-count 0)
        (current-target)
        (current-df-set-size 0)
        (definitions-table (make-hash-table)))
    (declare (type hash-table definitions-table work-table)
             (type fixnum iteration-count extra-count node-count current-df-set-size))
    (multiple-value-setq (definitions-table work-queue work-table)
      (initialize-flowgraph-queue flow-graph))
    ;; process worklist
    (loop :while (> (util.collections:queue-size work-queue) 0)
          :with node
          :do
            (setq node
                  (util.collections:dequeue work-queue))
            (setf (gethash node work-table)
                   nil)
            (incf iteration-count)
            (loop :for edge :in (edges node)
                  :do
                    (setq current-target
                          (target edge))
                    (setq current-df-set-size                     
                          (propagate-values node current-target))
                    ;; calculate dataflow effect
                    (calculate-dataflow-effect edge current-target definitions-table)
;                   (format t "~% Reaching definitions node ~A: ~A ~A"
;                           (id current-target)
;                           (if (instruction edge)
;                               (jana.java.jimple.conversion:jimple-statement (instruction edge))
;                               "branch")                          
;                           (with-output-to-string (s)
;                             (write-reaching-definitions current-target flow-graph s)))
                    ;; done -- process next element
                    (unless (or (= (hashset-count (df-values current-target))
                                   current-df-set-size)
                                (gethash current-target work-table))
                      (incf extra-count)
                      (setf (gethash current-target work-table) t)
                      (util.collections:enqueue work-queue current-target (out-degree current-target)))))
    (format t "~% RDA [r:~f p:~A/~A]." (/ iteration-count node-count) iteration-count node-count)
    (/ iteration-count node-count)))

(defclass dominator-graph ()
  ((virtual-root
    :ACCESSOR virtual-root
    :DOCUMENTATION "The virtual root of the dominator graph.")
   (root-node
    :ACCESSOR root-node
    :DOCUMENTATION "The root node in the host flow-graph.")
   (depthfirst-number
    :ACCESSOR depthfirst-number
    :TYPE fixnum
    :INITFORM 0
    :DOCUMENTATION "Counter used to enumerate nodes in depth-first order.")
   (size
    :TYPE hash-table
    :INITFORM (make-hash-table :test #'eq))
   (label ; top-ancestor
    :TYPE hash-table
    :INITFORM (make-hash-table :test #'eq)
    :DOCUMENTATION "A mapping from a node to the depthfirst number of those ancestor, whose semidominator has the minimum depth-first number.")
   (ancestor
    :TYPE hash-table
    :INITFORM (make-hash-table :test #'eq)
    :DOCUMENTATION "A mapping from a node to the depthfirst number of its ancestor in the dominator graph.")
   (parent
    :TYPE hash-table
    :INITFORM (make-hash-table :test #'eq)
    :DOCUMENTATION "A mapping from a node to its parent in the depth-first spanning tree.")
   (child
    :TYPE hash-table
    :INITFORM (make-hash-table :test #'eq)
    :DOCUMENTATION "A mapping from a node to its child in the depth-first spanning tree.")
   (semi-dominator-depthfirst-number
    :TYPE hash-table
    :INITFORM (make-hash-table :test #'eq)
    :DOCUMENTATION "A mapping from nodes to the depth-first number of the node's semi-dominator.")
   (depthfirst-number-node
    :TYPE hash-table
    :INITFORM (make-hash-table :test #'eq)
    :DOCUMENTATION "A mapping from depth-first numbers to nodes.")
   (immediate-dominator
    :TYPE hash-table
    :INITFORM (make-hash-table :test #'eq)
    :DOCUMENTATION "A mapping from nodes to immediate dominators of the nodes.")
   (node-buckets
    :TYPE hash-table
    :INITFORM (make-hash-table :test #'eq)
    :DOCUMENTATION "Buckets for nodes that have the same dominator.")) ;; can be associated with nodes -- e.g. as property of a node
  (:DOCUMENTATION ""))

;;; size

(defmacro size (dominator-graph node)
  "access macro"
  `(gethash ,node (slot-value ,dominator-graph 'size)))

(defmacro set-size (dominator-graph node value)
  "update macro"
  `(setf (gethash ,node (slot-value ,dominator-graph 'size))
         ,value))

(defsetf size set-size)

;;; label

(defmacro label (dominator-graph node)
  "access macro"
  `(gethash ,node (slot-value ,dominator-graph 'label)))

(defmacro set-label (dominator-graph node value)
  "update macro"
  `(setf (gethash ,node (slot-value ,dominator-graph 'label))
         ,value))

(defsetf label set-label)

;;; ancestor

(defmacro ancestor (dominator-graph node)
  "access macro"
  `(gethash ,node (slot-value ,dominator-graph 'ancestor)))

(defmacro set-ancestor (dominator-graph node value)
  "update macro"
  `(setf (gethash ,node (slot-value ,dominator-graph 'ancestor))
         ,value))

(defsetf ancestor set-ancestor)

;;; parent

(defmacro parent (dominator-graph node)
  "access macro"
  `(gethash ,node (slot-value ,dominator-graph 'parent)))

(defmacro set-parent (dominator-graph node value)
  "update macro"
  `(setf (gethash ,node (slot-value ,dominator-graph 'parent))
         ,value))

(defsetf parent set-parent)

;;; child

(defmacro child (dominator-graph node)
  "access macro"
  `(gethash ,node (slot-value ,dominator-graph 'child)))

(defmacro set-child (dominator-graph node value)
  "update macro"
  `(setf (gethash ,node (slot-value ,dominator-graph 'child))
         ,value))

(defsetf child set-child)

;;; semi-dominator

(defmacro semi-dominator-depthfirst-number (dominator-graph node)
  "access macro"
  `(gethash ,node (slot-value ,dominator-graph 'semi-dominator-depthfirst-number)))

(defmacro set-semi-dominator-depthfirst-number (dominator-graph node value)
  "update macro"
  `(setf (gethash ,node (slot-value ,dominator-graph 'semi-dominator-depthfirst-number))
         ,value))

(defsetf semi-dominator-depthfirst-number set-semi-dominator-depthfirst-number)

;;; depthfirst number -> node 

(defmacro depthfirst-number-node (dominator-graph number)
  "access macro"
  `(gethash ,number (slot-value ,dominator-graph 'depthfirst-number-node)))

(defmacro set-depthfirst-number-node (dominator-graph number value)
  "update macro"
  `(setf (gethash ,number (slot-value ,dominator-graph 'depthfirst-number-node))
         ,value))

(defsetf depthfirst-number-node set-depthfirst-number-node)

;;; immediate-dominator

(defmacro immediate-dominator (dominator-graph node)
  "access macro"
  `(gethash ,node (slot-value ,dominator-graph 'immediate-dominator)))

(defmacro set-immediate-dominator (dominator-graph node value)
  "update macro"
  `(setf (gethash ,node (slot-value ,dominator-graph 'immediate-dominator))
         ,value))

(defsetf immediate-dominator set-immediate-dominator)

;;; node-buckets

(defmacro node-buckets (dominator-graph node)
  "access macro"
  `(gethash ,node (slot-value ,dominator-graph 'node-buckets)))

(defmacro set-node-buckets (dominator-graph node value)
  "update macro"
  `(setf (gethash ,node (slot-value ,dominator-graph 'node-buckets))
         ,value))

(defsetf node-buckets set-node-buckets)


(defmethod make-dominator-graph ((self flow-graph))
  "Constructor"
  (let ((dominator-graph (make-instance 'dominator-graph))
        (virtual-root-node (make-instance (node-type self))))
    (setf (virtual-root dominator-graph)
          virtual-root-node)
    (setf (root-node dominator-graph)
          (root-node self))
    (loop :for node :in (nodes self)
          :do (setf (node-buckets dominator-graph node)
                    (util.collections::make-hash-set))
              (setf (semi-dominator-depthfirst-number dominator-graph node)
                    0))
    (setf (node-buckets dominator-graph virtual-root-node)
          (util.collections::make-hash-set))
    (setf (semi-dominator-depthfirst-number dominator-graph virtual-root-node)
          0)
    (setf (size dominator-graph virtual-root-node)
          0)
    (setf (label dominator-graph virtual-root-node)
          virtual-root-node)
    (setf (ancestor dominator-graph virtual-root-node)
          virtual-root-node)
    (setf (depthfirst-number dominator-graph)
          0)
  dominator-graph))

(defmethod initialize-dominator-graph ((self dominator-graph))
  "Initialize the dominator-graph in a depth first walk of the host-graph."
  (declare #.*standard-optimize-settings*)
  (let* ((current-node (root-node self))
         (node-queue `(,current-node))
         (depthfirst-number 0))
    (declare (type list node-queue)
             (type node current-node))
    (loop :while (> (length node-queue) 0)
      :do
       (setq current-node
             (pop node-queue))
       (incf (slot-value self 'depthfirst-number))
       (setq depthfirst-number
             (slot-value self 'depthfirst-number))
       (setf (semi-dominator-depthfirst-number self current-node)
             depthfirst-number)
       (setf (depthfirst-number-node self depthfirst-number)
             current-node)
       (setf (label self current-node)
             current-node)
       (setf (ancestor self current-node)
             (virtual-root self))
       (setf (child self current-node)
             (virtual-root self))
       (setf (size self current-node)
             1)
       (loop :for edge :in (edges current-node)
             :with target-node = (target edge)
             :when (= 0 (semi-dominator-depthfirst-number self target-node))
             :do (setf (parent self target-node)
                       current-node)
                 (setq node-queue
                       (nconc node-queue (list target-node)))))))


(defmethod dominators ((self flow-graph))
  (let ((dominator-graph (make-dominator-graph self))
        (current-parent-node)
        (current-label)
        (current-semi-dominator)
        (label-semi-dominator))
    (initialize-dominator-graph dominator-graph)
    ;; initial semidominators, store nodes with same semidominator in same bucket
    (loop :for current-depthfirst-number :from (depthfirst-number dominator-graph) :downto 2 :by 1
          :with current-node = (depthfirst-number-node dominator-graph current-depthfirst-number)
          :do
            (loop :for edge :in (edges current-node)
                  :with current-predecessor-node = (source edge)
                  :do
                    (setq current-label
                          (calculate-label dominator-graph current-predecessor-node))
                    (setq current-semi-dominator
                          (semi-dominator-depthfirst-number dominator-graph current-node))
                    (setq label-semi-dominator
                          (semi-dominator-depthfirst-number dominator-graph current-label))
                    (when (< label-semi-dominator
                             current-semi-dominator)
                          (setf (semi-dominator-depthfirst-number dominator-graph current-node)
                                label-semi-dominator)))
            (add-element
             (node-buckets dominator-graph
                           (depthfirst-number-node dominator-graph label-semi-dominator))
             current-node)
            (setq current-parent-node
                  (parent dominator-graph current-node))
            (link dominator-graph current-parent-node current-node)
            ;; immediate dominators for nodes in the bucket of the current parent-node
            (loop :while (> (length (node-buckets dominator-graph current-parent-node)) 0)
              :do
              (loop :for bucket-node :in (node-buckets dominator-graph current-parent-node)
                    :with bucket-node-label = (calculate-label dominator-graph bucket-node)
                    :do                  
                      (remove-element (node-buckets dominator-graph current-parent-node)
                                      bucket-node)
                      (if (< (semi-dominator-depthfirst-number dominator-graph bucket-node-label)
                             (semi-dominator-depthfirst-number dominator-graph bucket-node))
                          (setf (immediate-dominator dominator-graph bucket-node)
                                bucket-node-label)
                          (setf (immediate-dominator dominator-graph bucket-node)
                                current-parent-node)))))
    ;; 
    (loop :for current-depthfirst-number :from 2 :to (depthfirst-number dominator-graph)
          :with current-node = (depthfirst-number-node dominator-graph current-depthfirst-number)
          :with current-immediate-dominator = (immediate-dominator dominator-graph current-depthfirst-number)
          :do
            ;; unless immediate dominator and semi-dominator are the same
            (unless (same-p current-immediate-dominator
                            (depthfirst-number-node dominator-graph (semi-dominator-depthfirst-number dominator-graph current-node)))
              (setf (immediate-dominator dominator-graph current-node)
                    (immediate-dominator dominator-graph current-immediate-dominator)))) 
    (slot-value dominator-graph 'immediate-dominator)))

#.(declaim (inline calculate-label))
(defun calculate-label (dominator-graph current-node)
  "Determine the ancestor of the node CURRENT-NODE that has the
semidominator with the minimal depth-first number."
  (declare #.*standard-optimize-settings*
           (type dominator-graph dominator-graph)
           (type node current-node))
  (let ((ancestor-node (ancestor dominator-graph current-node))
        (current-label (label dominator-graph current-node))        
        (ancestor-label))
    (cond
      ((same-p ancestor-node
               (virtual-root dominator-graph))
       current-label)
      (t 
       (compress-path dominator-graph current-node)
       ;;
       (setq current-label (label dominator-graph current-node))
       (setq ancestor-label (label dominator-graph ancestor-node))
       ;;
       (if (>= (semi-dominator-depthfirst-number dominator-graph ancestor-label)
               (semi-dominator-depthfirst-number dominator-graph current-label))
           current-label
           ancestor-label)))))

#.(declaim (inline compress-path))
(defun compress-path (dominator-graph start-node)
  "Compresses the path from the node CURRENT-NODE to the
root of the depth-first spanning tree."
  (declare #.*standard-optimize-settings*
           (type dominator-graph dominator-graph)
           (type node start-node))
  (let ((current-ancestor-node (ancestor dominator-graph start-node))
        (current-node start-node)
        (node-stack `(,start-node))
        (ancestor-label)
        (current-label))
    (declare (type node current-ancestor-node current-node)
             (type list node-stack))
    ;; create stack -- (compress current-ancestor-node dominator-graph)
    (loop :unless (same-p current-ancestor-node
                          (virtual-root dominator-graph))
      :do
        (push current-ancestor-node
              node-stack)
        (setq current-ancestor-node
              (ancestor dominator-graph current-ancestor-node)))
    ;; unwind stack
    (loop :while (> (length node-stack) 0)
      :do (setq current-node
                (pop node-stack))
          (setq current-ancestor-node
                (ancestor dominator-graph current-node))
          (setq current-label (label dominator-graph current-node))
          (setq ancestor-label (label dominator-graph current-ancestor-node))
          (when (< (semi-dominator-depthfirst-number dominator-graph ancestor-label)
                   (semi-dominator-depthfirst-number dominator-graph current-label))
            (setf (label dominator-graph current-node)
                  ancestor-label))
          (setf (ancestor dominator-graph current-node)
                (ancestor dominator-graph current-ancestor-node)))))

(defun link (self parent-node node)
  (let ((current-node node)
        (tmp-node node)
        (other-size (size self node))
        (node-label (label self parent-node)))
    (declare (type node current-node tmp-node)
             (type fixnum other-size))
    (loop :with current-child-node = (child self current-node)
          :with current-grandchild-node = (child self current-child-node)
          :while (< (semi-dominator-depthfirst-number self node-label)
                    (semi-dominator-depthfirst-number self (label self current-child-node)))
          :do
            (cond ((>= (+ (size self current-node)
                          (size self current-grandchild-node))
                       (ash (size self current-child-node) 1))
                   (setf (ancestor self current-child-node)
                         current-node)
                   (setf (child self current-node)
                         current-grandchild-node))
                  (t
                   (setf (size self current-child-node)
                         (size self current-node))
                   (setf (ancestor self current-node)
                         current-child-node)
                   (setq current-node
                         current-child-node))))
    (setf (label self current-node)
          (label self node))
    (incf (size self parent-node)
          other-size)
    (when (< (size self parent-node) (ash other-size 1))
      ;; swap-values
      (setq tmp-node current-node)
      (setq current-node (child self parent-node))
      (setf (child self parent-node) tmp-node))
    (loop :until (same-p current-node (root-node self))
      :do
       (setf (ancestor self current-node)
             parent-node)
       (setq current-node
             (child self current-node)))))
                
 
;(defun dfs-dom (start-node)
;  (incf depthfirst-number)
;  (setf (gethash start-node semi-dominator-depthfirst-number-table)
;        depthfirst-number)
;  (setf (gethash depthfirst-number node-depthfirst-number-table)
;        start-node)
;  (setf (gethash depthfirst-number lable-table)
;        start-node)
;  (setf (gethash start-node ancestor-table)
;        virtual-root)
;  (setf (gethash start-node child-table)
;        virtual-root)
;  ;; init size
;  (setf (gethash start-node size-table)
;        1)
;  (loop :for edge :in (edges current-node)
;        :with target-node = (target edge)
;        :when (= 0 (gethash target-node semidominator-depthfirst-number-table))
;        :do (setf (gethash target-node parent-table)
;                  start-node)
;            (dfs-dom target-node)))
  
;;; Structural Analysis
;
;(defclass structural-analysis-node (node)
;  ((region
;    :ACCESSOR region
;    :DOCUMENTATION "The region containing the node.")))
;
;(defclass region-node (node graph)
;  ())
;
;(defclass block-node (region-node)
;  ())
;
;(defclass if-then-node (region-node)
;  ())
;
;(defclass if-then-else-node (region-node)
;  ())
;
;(defclass cond-node (region-node)
;  ()
;  (:DOCUMENTATION "Also case-node."))
;
;(defclass proper-loop-node (region-node)
;  ())
;
;(defclass self-loop-node (region-node)
;  ())
;
;(defclass while-loop-node (region-node)
;  ())
;
;(defclass natural-loop-node (region-node)
;  ())
;
;(defclass improper-region-node (region-node)
;  ())
;
;(defclass control-tree ()
;  ((nodes)))
;   
;
;(defmethod structural-analysis ((self graph) entry-node)
;  (let ((post-max 0)
;        (post-counter 1)
;        (post-table (make-hash-table :test eq))
;        (current-node entry-node)
;        (region)
;        (reach-under '()))
;    (declare (type fixnum post-max post-counter))
;    (loop :until (= (length (nodes self)) 0)
;      :do
;       (setq post-max
;             (depth-first-walk-postorder entry-node post-table post-max))
;       (while (and (> (length self) 1)
;                   (<= post-counter post-max))
;         (setq current-node
;               (gethash post-counter post-table))
;         ;; locate acyclic region
;         (setq region
;               (acyclic-region-type current-node))
;         (when region
;           (reduce-graph self region))
;         (unless region
;           (setq reach-under (list current-node))
;           (loop :for node :in (nodes self)
;                 :when (path-back node current-node)
;                 :do (push node reach-under))
;           (setq region
;                 (cyclic-region-type current-node reach-under))
;           (unless region
;             (incf post-counter)))))))
;
;(defmethod reduce-graph ((self graph) (region region-node))
;  (setf (nodes self)
;        (set-difference (nodes self)
;                        (nodes region)
;                        :test #'equalp)) ; replace against appropriate test
;  ;(push (structures self)
;  ;      region)
;  ;; set for all nodes in region the region -- chr: might be uneccessary 
;  ;(setf (gethash region (structure-nodes self))
;  ;      (nodes region)))
;  )
;
;(defmethod cyclic-region-type ((self node) reach-under)
;  (when (= 1 (length (edges self)))
;    (let ((edge))
;      (setq edge (first (edges self)))
;      (if (and (eq (source edge) self)
;               (eq (target edge) self))
;          (make-instance 'self-loop-node :nodes node)))))
;                
;                   
;
;(defun depth-first-walk-postorder (entry-node post-table post-max)
;  (declare (type graph graph)
;           (type node entry-node)
;           (type hash-table post-table)
;           (type fixnum post-max))
;  (let ((node-stack '())
;        (current-node entry-node))
;    (declare (type list node-stack)
;             (type node entry-node))
;    (push entry-node node-stack)
;    (while (> (length node-stack) 0)
;      (setq current-node (pop node-stack))
;      (setf (visited current-node) t)
;      (loop :for edge :in (edges current-node)
;            :with target-node = (target edge)
;            :unless (visited target-node)
;            :do (push target-node node-stack))
;      (incf post-max)
;      (setf (gethash post-max post-table)
;            current-node))
;    post-max))
;
;(defun depth-first-walk-recursively (graph current-node &key before-action after-action)
;  "The function used for recursive depth-first traversal."
;  (when before-action
;    (funcall before-action current-node))
;  (setf (visited current-node) t)
;  (loop :for edge :in (edges current-node)
;        :with target-node = (target edge)
;        :unless (visited target-node)
;        :do (when before-action
;              (funcall before-action target-node))
;        (depth-first-walk-recursively self target-node current-node
;                                      :before-action before-action :after-action after-action)
;        (when after-action
;          (funcall after-action target-node)))
;  (when after-action
;    (funcall after-action current-node)))
;
;(defmethod depth-first-walk ((self graph) &key before-action after-action)
;  "Entry point for the depth-first walk method.
;The keywords :before-action and :after-action can be used to provide functions
;that are called with the current-node as argument before the node has been visited,
;respectively after the node has been visited."
;  (loop :for node :in (nodes self)
;        :do (setf (visited node) nil))
;  (let ((current-node (start-node self)))
;    (when before-action
;      (funcall before-action current-node))
;    (setf (visited current-node) t)
;    (loop :for edge :in (edges current-node)
;          :with target-node = (target edge)
;          :unless (visited target-node)
;          :do (when before-action
;                (funcall before-action target-node))
;              (depth-first-walk-recursively self target-node current-node
;                                            :before-action before-action :after-action after-action)
;              (when after-action
;                (funcall after-action target-node)))
;    (when after-action
;      (funcall after-action current-node))))
    