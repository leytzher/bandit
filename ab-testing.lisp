(ql:quickload :py4cl)
(defpackage :ab-testing
  (:use :cl :py4cl))

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
	 (trial-numbers nil)
	 (average-rewards nil))
    (dotimes (i n-test)
      (multiple-value-bind (ad-chosen reward) (select-random-ad)
	(incf (aref n ad-chosen))
	(setf (aref q-values ad-chosen)
	      (+ (aref q-values ad-chosen)
		 (/ (- reward (aref q-values ad-chosen))
		    (aref n ad-chosen))))
	(incf total-reward reward)
	(push (1+ i) trial-numbers)
	(push (/ total-reward (1+ i)) average-rewards)))
    (values
	    (reverse trial-numbers)
	    (reverse average-rewards)
	    (position (reduce #'max q-values) q-values)
	    (reduce #'max q-values))))


(py4cl:python-exec "
import plotly.graph_objects as go
import plotly.io as pio

def create_plot(x, y, filename):
    fig = go.Figure(data=go.Scatter(x=x, y=y, mode='lines+markers'))
    fig.update_layout(title='Average Reward Over Time',
                      xaxis_title='Number of Trials',
                      yaxis_title='Average Reward')
    pio.write_html(fig, file=filename, auto_open=True)
")


(defun run-and-plot-campaign (n-test n-prod filename)
  (multiple-value-bind (x-data y-data best-ad best-q-value)
      (play-campaign n-test n-prod)
    (py4cl:python-call "create_plot" x-data y-data filename)
    (format t "Best ad: ~A, Q-value: ~A~%" best-ad best-q-value)
    (format t "Plot saved to ~A~%" filename)))

(run-and-plot-campaign 100000 90000 "campaign_plot.html")

	 


