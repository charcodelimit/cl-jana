(in-package :io.zip)

;; chr: factor out --> put into cl-utilities or something!
(declaim (inline compare-vectors))
(defun compare-vectors (vec-1 vec-2 &key (test #'equal))
  "Compares to vectors VEC-1 and VEC-2 using TEST.
 The default  test is #'equal."
  (declare #.*standard-optimize-settings*
           (type vector vec-1 vec-2))
    (reduce #'(lambda (x y) (and x y)) (map 'vector test vec-1 vec-2)))

;; -------- DEBUG MACROS ---------

(defun set-debug-mode ()
  (push +FEATURE-DEBUG-MODE-SYMBOL+ *features*))

(defmacro debug-mode ()
  (if (member +FEATURE-DEBUG-MODE-SYMBOL+ *features*)
      't
      'nil))

(defmacro verbose-mode ()
  (if (or
       (member +FEATURE-VERBOSE-MODE-SYMBOL+ *features*)
       (debug-mode))
      't
      'nil))

;; --------- IF* MACRO ---------

(defvar if*-keyword-list '("then" "thenret" "else" "elseif"))

(defmacro if* (&rest args)
   (do ((xx (reverse args) (cdr xx))
	(state :init)
	(elseseen nil)
	(totalcol nil)
	(lookat nil nil)
	(col nil))
       ((null xx)
	(cond ((eq state :compl)
	       `(cond ,@totalcol))
	      (t (error "if*: illegal form ~s" args))))
       (cond ((and (symbolp (car xx))
		   (member (symbol-name (car xx))
			   if*-keyword-list
			   :test #'string-equal))
	      (setq lookat (symbol-name (car xx)))))

       (cond ((eq state :init)
	      (cond (lookat (cond ((string-equal lookat "thenret")
				   (setq col nil
					 state :then))
				  (t (error
				      "if*: bad keyword ~a" lookat))))
		    (t (setq state :col
			     col nil)
		       (push (car xx) col))))
	     ((eq state :col)
	      (cond (lookat
		     (cond ((string-equal lookat "else")
			    (cond (elseseen
				   (error
				    "if*: multiples elses")))
			    (setq elseseen t)
			    (setq state :init)
			    (push `(t ,@col) totalcol))
			   ((string-equal lookat "then")
			    (setq state :then))
			   (t (error "if*: bad keyword ~s"
					      lookat))))
		    (t (push (car xx) col))))
	     ((eq state :then)
	      (cond (lookat
		     (error
		      "if*: keyword ~s at the wrong place " (car xx)))
		    (t (setq state :compl)
		       (push `(,(car xx) ,@col) totalcol))))
	     ((eq state :compl)
	      (cond ((not (string-equal lookat "elseif"))
		     (error "if*: missing elseif clause ")))
	      (setq state :init)))))

