;; (with-output-to-string (str) (output-float-aux 2e10  str -3 8))
;; 22.1.3.1.3 Printing Floats

(defun output-float-aux (x stream e-min e-max)
  (multiple-value-bind (e string)
      (sb-impl::flonum-to-digits x)
    (cond
      ((< e-min e e-max)
       (if (plusp e)
           (progn
             (write-string string stream :end (min (length string) e))
             (dotimes (i (- e (length string)))
               (write-char #\0 stream))
             (write-char #\. stream)
             (write-string string stream :start (min (length string) e))
             (when (<= (length string) e)
               (write-char #\0 stream))
             (sb-impl::print-float-exponent x 0 stream))
           (progn
             (write-string "0." stream)
             (dotimes (i (- e))
               (write-char #\0 stream))
             (write-string string stream)
             (sb-impl::print-float-exponent x 0 stream))))
      (t (write-string string stream :end 1)
         (write-char #\. stream)
	 (format t "String: ~A" string)
         (write-string string stream :start 1)
         (when (= (length string) 1)
               (write-char #\0 stream)) ;chr "at least one digit after the decimal point."
         (print-float-exponent x (1- e) stream)))))

(defun print-float-exponent (x exp stream)
  (declare (type float x) (type integer exp) (type stream stream))
  (let ((*print-radix* nil))
    (if (typep x *read-default-float-format*)
        (unless (eql exp 0) ; "except that if the format of the number matches that specified by *read-default-float-format*,"
          (format stream "E~D" exp)) ; chr "then the exponent marker E is used."
        (format stream "~C~D"
                (etypecase x
                  (single-float #\f)
                  (double-float #\d)
                  (short-float #\s)
                  (long-float #\L))
                exp))))


(assert (string= "1.0E12" (princ-to-string 1.0E12)))
