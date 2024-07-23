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
	(reward (display-ad (aref *ads* selection))))
    (values selection reward)))

(defun play-campaign (n-test n-prod)
  (let* ((n-ads (length *ads*))
	 (q-values (make-array `(,n-ads) :initial-element 0.0))
	 (n (make-array `(,n-ads) :initial-element 0))
	 (total-reward 0)
	 (average-rewards nil))
    (dotimes (i n-test)
      (multiple-value-bind (ad-chosen reward) (select-random-ad)
	(incf (aref n ad-chosen))
	(setf (aref q-values ad-chosen)
	      (+ (aref q-values ad-chosen)
		 (/ (- reward (aref q-values ad-chosen))
		    (aref n ad-chosen))))
	(incf total-reward reward)
	(push (/ total-reward (1+ i)) average-rewards)))
    (values (reverse average-rewards)
	    (position (reduce #'max q-values) q-values)
	    (reduce #'max q-values))))




    
	 


