(in-package :jana.metamodel)

(declaim (inline load-metamodel-file))
(defun load-metamodel-file (filename)
  (let ((in-pathname (merge-pathnames filename))
	(file-contents ()))
    (declare (type list file-contents))
    (with-open-file (s in-pathname)
      (setf file-contents (read s)))
    file-contents))

(declaim (inline evaluate-analysis-results))
(defun evaluate-analysis-results (file-contents)
  (eval (third file-contents)))

(defun test-file (filename)
  (evaluate-analysis-results
   (load-metamodel-file filename)))

(defun write-to-jimple-file (filename string)
  (let ((out-p
	 (merge-pathnames (concatenate 'string filename ".jimple"))))
    (format t "Writing to ~S" out-p)
    (with-open-file (s out-p :direction :output :if-exists :supersede)
      (write-string string s)))
  t)

;;; SBCL
;(time (format t "" (write-to-jimple-file "ORBUtilSystemException-jana" (jimple-statement (test-file "test/ORBUtilSystemException-pp.lisp")))))
;; 
;; caught STYLE-WARNING:
;;   Too many arguments (1) to FORMAT "": uses at most 0.
;;   See also:
;;     The ANSI Standard, Section 22.3.10.2
;; 
;; compilation unit finished
;;   caught 1 STYLE-WARNING condition
;Writing to #P"/home/chrissi/workspace/projects/essay/cl-jana/ORBUtilSystemException-jana.jimple"
;Evaluation took:
;  26.504 seconds of real time
;  26.333646 seconds of total run time (24.941559 user, 1.392087 system)
;  [ Run times consist of 2.056 seconds GC time, and 24.278 seconds non-GC time. ]
;  99.36% CPU
;  293,925 forms interpreted
;  48,468,026,431 processor cycles
;  2,923,732,672 bytes consed
;NIL

;(time (format t "" (write-to-jimple-file "ORBUtilSystemException-jana" (jimple-statement (test-file "test/ORBUtilSystemException-pp.lisp")))))
;; 
;; caught STYLE-WARNING:
;;   Too many arguments (1) to FORMAT "": uses at most 0.
;;   See also:
;;     The ANSI Standard, Section 22.3.10.2
;; 
;; compilation unit finished
;;   caught 1 STYLE-WARNING condition
;Writing to #P"/home/chrissi/test/cl-jana_0/ORBUtilSystemException-jana.jimple"
;Evaluation took:
;  2.124 seconds of real time
;  2.108132 seconds of total run time (2.032127 user, 0.076005 system)
;  [ Run times consist of 0.368 seconds GC time, and 1.741 seconds non-GC time. ]
;  99.25% CPU
;  293,925 forms interpreted
;  3,884,648,394 processor cycles
;  166,394,352 bytes consed


;(time (format t "" (write-to-jimple-file "ORBUtilSystemException-jana" (with-output-to-string (s) (jimple-statement (test-file "test/ORBUtilSystemException-pp.lisp") s)))))
;; 
;; caught STYLE-WARNING:
;;   Too many arguments (1) to FORMAT "": uses at most 0.
;;   See also:
;;     The ANSI Standard, Section 22.3.10.2
;; 
;; compilation unit finished
;;   caught 1 STYLE-WARNING condition
;Writing to #P"/home/chrissi/workspace/projects/essay/cl-jana/ORBUtilSystemException-jana.jimple"
;Evaluation took:
;  1.905 seconds of real time
;  1.892118 seconds of total run time (1.792112 user, 0.100006 system)
;  [ Run times consist of 0.440 seconds GC time, and 1.453 seconds non-GC time. ]
;  99.32% CPU
;  293,925 forms interpreted
;  3,482,774,273 processor cycles
;  237,685,552 bytes consed
;NIL

;; optimized stream output + reduced signature
;(time (format t "" (write-to-jimple-file "ORBUtilSystemException-jana" (with-output-to-string (s) (jimple-statement (test-file "test/ORBUtilSystemException-pp.lisp") s)))))
;; 
;; caught STYLE-WARNING:
;;   Too many arguments (1) to FORMAT "": uses at most 0.
;;   See also:
;;     The ANSI Standard, Section 22.3.10.2
;; 
;; compilation unit finished
;;   caught 1 STYLE-WARNING condition
;Writing to #P"/home/chrissi/workspace/projects/essay/cl-jana/ORBUtilSystemException-jana.jimple"
;Evaluation took:
;  1.448 seconds of real time
;  1.428089 seconds of total run time (1.380086 user, 0.048003 system)
;  [ Run times consist of 0.252 seconds GC time, and 1.177 seconds non-GC time. ]
;  98.62% CPU
;  255,748 forms interpreted
;  654 lambdas converted
;  2,646,645,067 processor cycles
;  63,167,256 bytes consed
;NIL

;; minimal signature
;(time (format t "" (write-to-jimple-file "ORBUtilSystemException-jana" (with-output-to-string (s) (jimple-statement (test-file "test/ORBUtilSystemException-pp.lisp") s)))))
;; 
;; caught STYLE-WARNING:
;;   Too many arguments (1) to FORMAT "": uses at most 0.
;;   See also:
;;     The ANSI Standard, Section 22.3.10.2
;; 
;; compilation unit finished
;;   caught 1 STYLE-WARNING condition
;Writing to #P"/home/chrissi/workspace/projects/essay/cl-jana/ORBUtilSystemException-jana.jimple"
;Evaluation took:
;  1.379 seconds of real time
;  1.380087 seconds of total run time (1.324083 user, 0.056004 system)
;  [ Run times consist of 0.456 seconds GC time, and 0.925 seconds non-GC time. ]
;  100.07% CPU
;  216,077 forms interpreted
;  40 lambdas converted
;  2,523,190,461 processor cycles
;  49,653,880 bytes consed
;NIL

;* (time (test-file "test/ORBUtilSystemException-pp.lisp"))
;
;Evaluation took:
;  1.261 seconds of real time
;  1.256079 seconds of total run time (1.196075 user, 0.060004 system)
;  [ Run times consist of 0.128 seconds GC time, and 1.129 seconds non-GC time. ]
;  99.60% CPU
;  293,925 forms interpreted
;  614 lambdas converted
;  2,306,453,083 processor cycles
;  48,031,816 bytes consed
;#<JAVA-CLASS-DECLARATION {B472E61}>


;;; CLISP
;(time (format t "" (write-to-jimple-file "ORBUtilSystemException-jana" (jimple-statement (test-file "test/ORBUtilSystemException-pp.lisp")))))
;Writing to #P"ORBUtilSystemException-jana.jimple"
;Real time: 7.834902 sec.
;Run time: 7.828489 sec.
;Space: 985197832 Bytes
;GC: 222, GC time: 2.124144 sec.
;NIL

; (time (format t "" (write-to-jimple-file "ORBUtilSystemException-jana" (jimple-statement (test-file "test/ORBUtilSystemException-pp.lisp")))))
;Writing to #P"ORBUtilSystemException-jana.jimple"
;Real time: 5.459852 sec.
;Run time: 5.44034 sec.
;Space: 346084488 Bytes
;GC: 77, GC time: 0.93205 sec.
;NIL

;(time (format t "" (write-to-jimple-file "ORBUtilSystemException-jana" (with-output-to-string (s) (jimple-statement (test-file "test/ORBUtilSystemException-pp.lisp") s)))))
;Writing to #P"ORBUtilSystemException-jana.jimple"
;Real time: 5.698561 sec.
;Run time: 5.676355 sec.
;Space: 356388072 Bytes
;GC: 75, GC time: 0.860054 sec.
;NIL

;; optimized stream-output with write-sequence and write-char + reduced signature
;(time (format t "" (with-output-to-string (s) (jimple-statement (test-file "test/ORBUtilSystemException-pp.lisp") s))))
;Real time: 4.988895 sec.
;Run time: 4.97631 sec.
;Space: 295058984 Bytes
;GC: 41, GC time: 1.004065 sec.
;NIL

;; full signature (signature <name> <qualified-name> <file-name>
;(time (test-file "test/ORBUtilSystemException-pp.lisp"))
;Real time: 1.617105 sec.
;Run time: 1.616102 sec.
;Space: 32524368 Bytes
;GC: 9, GC time: 0.360025 sec.
;#<JAVA-CLASS-DECLARATION #x000334944E28>

;; reduced signature (signature <name> <qualified-name>)
;(time (test-file "test/ORBUtilSystemException-pp.lisp"))
;Real time: 1.36215 sec.
;Run time: 1.360085 sec.
;Space: 29869496 Bytes
;GC: 4, GC time: 0.244014 sec.
;#<JAVA-CLASS-DECLARATION #x0003353F8A38>

;; minimum signature (signature <qualified-name>)
;(time (test-file "test/ORBUtilSystemException-pp.lisp"))
;Real time: 1.141428 sec.
;Run time: 1.140072 sec.
;Space: 27721088 Bytes
;GC: 7, GC time: 0.204016 sec.
;#<JAVA-CLASS-DECLARATION #x00033454AA80>
