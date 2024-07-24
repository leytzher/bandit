(ql:quickload :py4cl)
;;; Action selection using Upper Confidence Bounds

(defpackage :ucb
  (:use :cl :py4cl))
(in-package :ucb)
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

;;; At each time step we select the action that has the highest potential
;;; for reward.
;;; The potential of the action is the sum of the action value estimate
;;; and a measure of the uncertainty of this estimate.

;;; A_t = argmax(Qt(a) + c sqrt(ln(t)/Nt(a))

(defun arange (n)
  (make-array n :initial-contents
	      (loop for i from 0 below n collect i)))

(defun ucb1-campaign (n-prod c)
  (let* ((n-ads (length *ads*))
         (ad-indices (make-array n-ads :initial-contents (loop for i below n-ads collect i)))
         (n (make-array n-ads :initial-element 0))
         (q (make-array n-ads :initial-element 0.0))
         (total-reward 0.0)
         (avg-rewards nil))
    (dotimes (ti n-prod)
      (let ((ad-chosen
              (if (find 0 n)
                  (aref ad-indices (random (count 0 n)))
                  (let ((uncertainty (map 'vector 
                                          (lambda (ni) (sqrt (/ (log (1+ ti)) ni)))
                                          n)))
                    (position (reduce #'max (map 'vector #'+ q (map 'vector (lambda (u) (* c u)) uncertainty)))
                              (map 'vector #'+ q (map 'vector (lambda (u) (* c u)) uncertainty)))))))
        (let ((reward (display-ad (aref *ads* ad-chosen))))
          (incf (aref n ad-chosen))
          (setf (aref q ad-chosen) 
                (+ (aref q ad-chosen)
                   (/ (- reward (aref q ad-chosen))
                      (aref n ad-chosen))))
          (incf total-reward reward)
          (push (/ total-reward (1+ ti)) avg-rewards))))
    (reverse avg-rewards)))


(py4cl:python-exec "
import plotly.graph_objects as go
import plotly.io as pio

def create_plot(data, filename):
    fig = go.Figure()
    for c, avg_rewards in data:
        fig.add_trace(go.Scatter(y=avg_rewards, mode='lines', name=f'c={c}'))
    
    fig.update_layout(title='UCB1 Performance for Different c Values',
                      xaxis_title='Number of Trials',
                      yaxis_title='Average Reward')
    pio.write_html(fig, file=filename, auto_open=True)
")

(defun run-and-plot-ucb1 (n-prod c-values filename)
  (let ((results (loop for c in c-values
                       collect (list c (ucb1-campaign n-prod c)))))
    (py4cl:python-call "create_plot" results filename)
    (format t "Plot saved to: ~A~%" filename)))

;; Run the simulations and create the plot
(run-and-plot-ucb1 100000 '(0.1 1.0 10.0) "ucb1_comparison.html")

