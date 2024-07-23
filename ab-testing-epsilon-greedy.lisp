(ql:quickload :py4cl)
(defpackage :ab-testing-epsilon-greedy
  (:use :cl :py4cl))

(in-package :ab-testing-epsilon-greedy)

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


(defun play (&key epsilon n-prod)
  (let* ((n-ads (length *ads*))
         (q-values (make-array `(,n-ads) :initial-element 0.0))
         (n-values (make-array `(,n-ads) :initial-element 0))
         (total-reward 0.0)
         (avg-rewards nil))
    (dotimes (i n-prod)
      (let ((ad-chosen (if (<= (random 1.0) epsilon)
                           (random n-ads)
                           (position (reduce #'max q-values) q-values))))
        (multiple-value-bind (selection reward) (select-ad ad-chosen)
          (declare (ignore selection))
          (incf (aref n-values ad-chosen))
          (setf (aref q-values ad-chosen)
                (+ (aref q-values ad-chosen)
                   (/ (- reward (aref q-values ad-chosen))
                      (aref n-values ad-chosen))))
          (incf total-reward reward)
          (push (/ total-reward (1+ i)) avg-rewards))))
    (values (reverse avg-rewards) q-values)))

(defun select-ad (ad-index)
  (let ((reward (display-ad (aref *ads* ad-index))))
    (values ad-index reward)))

(py4cl:python-exec "
import plotly.graph_objects as go
import plotly.io as pio

def create_plot(x, y, epsilon, filename):
    fig = go.Figure(data=go.Scatter(x=x, y=y, mode='lines+markers'))
    fig.update_layout(
        title=f'Average Reward Over Time (Epsilon: {epsilon})',
        xaxis_title='Number of Trials',
        yaxis_title='Average Reward'
    )
    pio.write_html(fig, file=filename, auto_open=True)
")

(defun simulate-and-plot (epsilon n-prod filename)
  (multiple-value-bind (avg-rewards q-values) (play :epsilon epsilon :n-prod n-prod)
    (let ((x-data (loop for i from 1 to n-prod collect i)))
      (py4cl:python-call "create_plot" x-data avg-rewards epsilon filename)
      (format t "Simulation complete. Results:~%")
      (format t "Final Q-values: ~A~%" q-values)
      (format t "Best ad: ~A~%" (position (reduce #'max q-values) q-values))
      (format t "Plot saved to: ~A~%" filename))))

(simulate-and-plot 0.1 100000 "ab_test_e_greedy_results.html")

(py4cl:python-exec "
import plotly.graph_objects as go
import plotly.io as pio

def create_multi_plot(data, filename):
    fig = go.Figure()
    for eps, x, y in data:
        fig.add_trace(go.Scatter(x=x, y=y, mode='lines', name=f'Epsilon: {eps}'))
    
    fig.update_layout(
        title='Average Reward Over Time for Different Epsilon Values',
        xaxis_title='Number of Trials',
        yaxis_title='Average Reward'
    )
    pio.write_html(fig, file=filename, auto_open=True)
")

(defun simulate-multiple-epsilons (epsilons n-prod filename)
  (let ((plot-data '()))
    (dolist (epsilon epsilons)
      (multiple-value-bind (avg-rewards q-values) (play :epsilon epsilon :n-prod n-prod)
        (let ((x-data (loop for i from 1 to n-prod collect i)))
          (push (list epsilon x-data avg-rewards) plot-data)
          (format t "Simulation complete for epsilon ~A~%" epsilon)
          (format t "Final Q-values: ~A~%" q-values)
          (format t "Best ad: ~A~%~%" (position (reduce #'max q-values) q-values)))))
    
    (py4cl:python-call "create_multi_plot" (reverse plot-data) filename)
    (format t "Plot saved to: ~A~%" filename)))

(simulate-multiple-epsilons '(0.01 0.05 0.1 0.2) 100000 "ab_test_multi_epsilon.html")
