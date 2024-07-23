(defpackage :bandit
  (:use :cl))

(declaim (optimize  (debug 3) (speed 0) (space 0)))

(in-package :bandit)

(defclass bandit ()
  ((bandit-mean :accessor bandit-mean
		:initarg :bandit-mean
		:initform 1.0
		:type real)
   (bandit-std :accessor bandit-std
	       :initarg :bandit-std
	       :initform 0.0
	       :type real)))

(defun box-muller-transform ()
  (let* ((u1 (random 1.0))
	 (u2 (random 1.0))
	 (r (sqrt (* -2 (log u1))))
	 (theta (* 2 pi u2)))
    (values (* r (cos theta))
	    (* r (sin theta)))))	 

(defun make-bandit (mean std)
  (make-instance 'bandit :bandit-mean mean :bandit-std std))

(defun random-normal (mean std-dev)
  (multiple-value-bind (z1 z2) (box-muller-transform)
    (declare (ignore z2))
    (+ mean (* std-dev z1))))

(defmethod pull-lever ((object bandit))
  (random-normal (bandit-mean object)
		 (bandit-std object)))

(defun play-game (bandits)
  ())
