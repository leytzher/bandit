(defpackage :ab-testing
  (:use :cl))

(in-package :ab-testing)

(defclass bernoulli-bandit ()
  ((probability :accessor probability
		:initarg :probability
		:initform 0.5
		:type real)))

(defun make-bandit (&key probability)
  (make-instance 'bernoulli-bandit :probability probability))

(defmethod display-ad ((bandit bernoulli-bandit))
  (sample-bernoulli (probability bandit)))

(defun sample-bernoulli (probability)
  (if (< (random 1.0) probability) 1.0 0.0))

;;; Define ads as bandits
(defvar *ads* (vector
	       (make-bandit :probability 0.004)
	       (make-bandit :probability 0.016)
	       (make-bandit :probability 0.02)
	       (make-bandit :probability 0.028)
	       (make-bandit :probability 0.031)))

(defun select-random-ad ()
  (let* ((selection (random (length *ads*)))
	(reward (display-add (aref *ads* selection))))
    (values selection reward)))



