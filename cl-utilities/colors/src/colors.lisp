;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.colors; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        colors.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; Created: Fri Sep 18 11:37:40 2009 (z)
;;; 
;;; Last-Updated: Fri Sep 18 15:51:03 2009 (z)
;;;           By: Christian Hofmann
;;;****************************************************************************

(in-package :UTIL.COLORS)

(defclass color ()
  ()
  (:DOCUMENTATION "Abstract Superclass."))

#.(declaim (inline color-p))
(defun color-p (value)
  "RETURNS t if the argument VALUE is of type color."
  (typep value 'color))

;;;;
;;;; rgb
;;;;

(defclass rgb (color)
  ((red :initform 0 :type (real 0 1) :initarg :red :accessor red)
   (green :initform 0 :type (real 0 1) :initarg :green :accessor green)
   (blue :initform 0 :type (real 0 1) :initarg :blue :accessor blue)))

(defmethod print-object ((obj rgb) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (red green blue) obj
      (format stream "red: ~a  green: ~a  blue: ~a" red green blue))))

(defmethod print-hex ((obj rgb) stream)
  "Prints a representation of the rgb-object OBJ
as hexadecimal value to the output-stream STREAM."
  (with-slots (red green blue) obj
    (format stream "#~X~X~X"  (round (* red 255)) (round (* green 255)) (round (* blue 255)))))

(defmethod make-load-form ((obj rgb) &optional environment)
  (make-load-form-saving-slots obj :environment environment))

;;;;
;;;; rgba
;;;;

(defclass rgba (rgb)
  ((alpha :initform 1 :type (real 0 1) :initarg :alpha :accessor alpha)))

(defmethod print-object ((obj rgba) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (red green blue alpha) obj
      (format stream "red: ~a  green: ~a  blue: ~a  alpha: ~a"
	      red green blue alpha))))

(defgeneric add-alpha (color alpha)
  (:documentation "Add an alpha channel to a given color."))

(defmethod add-alpha ((color rgb) alpha)
  (make-instance 'rgba
		 :red (red color)
		 :green (green color)
		 :blue (blue color)
		 :alpha alpha))

;;;;
;;;; hsv
;;;;

(defclass hsv (color)
  ((hue :initform 0 :type (real 0 360) :initarg :hue :accessor hue)
   (saturation :initform 0 :type (real 0 1) :initarg :saturation
						     :accessor saturation)
   (value :initform 0 :type (real 0 1) :initarg :value :accessor value)))

(defmethod print-object ((obj hsv) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (hue saturation value) obj
      (format stream "hue: ~a  saturation: ~a  value: ~a"
	      hue saturation value))))

(defmethod print-hex ((obj hsv) stream)
  "Prints a representation of the rgb-object OBJ
as hexadecimal rgb values to the output-stream STREAM."
  (multiple-value-bind (red green blue)
      (hsv->rgb-value obj)
    (format stream "#~X~X~X"  (round (* red 255)) (round (* green 255)) (round (* blue 255)))))

(defun normalize-hue (hue)
  "Normalize hue into the interval [0,360)."
  (mod hue 360))

;;;;
;;;; conversions
;;;;

(defun rgb->hsv-value (rgb &optional (undefined-hue 0))
  "RETURNS the hue, saturation, and value components
of converting the rgb object RGB to the hsv color model."
  (with-slots (red green blue) rgb
    (let* ((value (max red green blue))
	   (delta (- value (min red green blue)))
	   (saturation (if (plusp value)
			   (/ delta value)
			   0)))
      (flet ((normalize (constant right left)
	       (let ((hue (+ constant (/ (* 60 (- right left)) delta))))
		 (if (minusp hue)
		     (+ hue 360)
		     hue))))
        (values
          (cond
		((zerop saturation) undefined-hue) ; undefined
		((= red value) (normalize 0 green blue)) ; dominant red
		((= green value) (normalize 120 blue red)) ; dominant green
		(t (normalize 240 red green)))
          saturation
          value)))))

(defun rgb->hsv (rgb &optional (undefined-hue 0))
  "Convert RGB to HSV representation.  When hue is undefined
\(saturation is zero), undefined-hue will be assigned."
  (multiple-value-bind (hue saturation value)
      (rgb->hsv-value rgb undefined-hue)
      (make-instance 'hsv
        :hue hue
        :saturation saturation
        :value value)))

(defun hsv->rgb-value (hsv)
  "RETURNS the red, green, and blue components
of converting the hsv object HSV to the rgb color model."
  (with-slots (hue saturation value) hsv
    ;; if saturation=0, color is on the gray line
    (when (zerop saturation)
      (return-from hsv->rgb-value (values value value value)))
    ;; nonzero saturation: normalize hue to [0,6)
    (let ((h (/ (normalize-hue hue) 60)))
      (multiple-value-bind (quotient remainder) (floor h)
	(let ((p (* value (- 1 saturation)))
	      (q (* value (- 1 (* saturation remainder))))
	      (r (* value (- 1 (* saturation (- 1 remainder))))))
	  (multiple-value-bind (red green blue)
	      (case quotient
		(0 (values value r p))
		(1 (values q value p))
		(2 (values p value r))
		(3 (values p q value))
		(4 (values r p value))
		(t (values value p q)))
	    (values red green blue)))))))

(defun hsv->rgb (hsv)
  "Convert HSV to RGB representation.  When saturation is zero, hue is
ignored."
  (multiple-value-bind (red green blue)
      (hsv->rgb-value hsv)
    (make-instance 'rgb
                   :red red
                   :green green
                   :blue  blue)))

;;;;
;;;; conversion with generic functions
;;;;

(defgeneric ->hsv (color &optional undefined-hue))

(defmethod ->hsv ((color rgb) &optional (undefined-hue 0))
  (rgb->hsv color undefined-hue))

(defmethod ->hsv ((color hsv) &optional undefined-hue)
  (declare (ignore undefined-hue))
  color)

(defgeneric ->rgb (color))

(defmethod ->rgb ((color rgb))
  color)

(defmethod ->rgb ((color hsv))
  (hsv->rgb color))

;;;;
;;;; convex combinations
;;;;

(defun convex-combination (a b alpha)
  "Convex combination (1-alpha)*a+alpha*b."
  (declare ((real 0 1) alpha))
  (+ (* (- 1 alpha) a) (* alpha b)))

(defun hue-combination (hue1 hue2 alpha &optional (positivep t))
  "Return a convex combination of hue1 (with weight 1-alpha) and
hue2 \(with weight alpha), in the positive or negative direction
on the color wheel."
  (cond
    ((and positivep (> hue1 hue2))
     (normalize-hue (convex-combination hue1 (+ hue2 360) alpha)))
    ((and (not positivep) (< hue1 hue2))
     (normalize-hue (convex-combination (+ hue1 360) hue2 alpha)))
    (t (convex-combination hue1 hue2 alpha))))
		  
(defmacro with-convex-combination ((cc instance1 instance2 alpha)
				&body body)
  "Wrap body in a macrolet so that (cc #'accessor) returns the
convex combination of the slots of instance1 and instance2
accessed by accessor."
  `(macrolet ((,cc (accessor)
	       (once-only (accessor)
		 `(convex-combination (funcall ,accessor ,',instance1)
				      (funcall ,accessor ,',instance2)
				      ,',alpha))))
     ,@body))
  
(defun rgb-combination (rgb1 rgb2 alpha)
  "Convex combination in RGB space."
  (with-convex-combination (cc rgb1 rgb2 alpha)
    (make-instance 'rgb :red (cc #'red) :green (cc #'green) :blue (cc #'blue))))

(defun rgba-combination (rgba1 rgba2 alpha)
  "Convex combination in RGBA space."
  (with-convex-combination (cc rgba1 rgba2 alpha)
    (make-instance 'rgba :red (cc #'red) 
		   :green (cc #'green) :blue (cc #'blue)
		   :alpha (cc #'alpha))))

(defun hsv-combination (hsv1 hsv2 alpha &optional (positivep t))
  (with-convex-combination (cc hsv1 hsv2 alpha)
    (make-instance 'hsv
     :hue (hue-combination (hue hsv1) (hue hsv2) alpha positivep)
     :saturation (cc #'saturation) :value (cc #'value))))
