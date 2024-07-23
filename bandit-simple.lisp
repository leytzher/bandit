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

(defun make-bandit (&key mean std)
  (make-instance 'bandit :bandit-mean mean :bandit-std std))

(defun random-normal (mean std-dev)
  (multiple-value-bind (z1 z2) (box-muller-transform)
    (declare (ignore z2))
    (+ mean (* std-dev z1))))

(defmethod pull-lever ((object bandit))
  (random-normal (bandit-mean object)
		 (bandit-std object)))


(defclass gaussian-bandit-game ()
  ((bandits :accessor bandits :initarg :bandits)
   (rewards :accessor rewards :initform nil)
   (total-reward :accessor total-reward :initform 0)
   (n-played :accessor n-played :initform 0)))

(defun make-gaussian-bandit-game (&key bandits)
  "Create bandit game"
  (make-instance 'gaussian-bandit-game :bandits bandits))

(defmethod play ((game gaussian-bandit-game) choice)
  (with-slots (bandits rewards total-reward n-played) game
    (let ((reward (pull-lever (nth (1- choice) bandits))))
      (push reward rewards)
      (incf total-reward reward)
      (incf n-played)
      reward)))

(defmethod user-play ((game gaussian-bandit-game))
  (reset-game game)
  (format t "Game started. Enter 0 to end the game.~%")
  (loop
    (format t "~%-- Round ~A --~%" (1+ (n-played game)))
    (format t "Choose a machine from 1 to ~A: " (length (bandits game)))
    (finish-output)
    (let ((choice (parse-integer (read-line) :junk-allowed t)))
      (cond
        ((or (null choice) (zerop choice))
         (return))
        ((and (> choice 0) (<= choice (length (bandits game))))
         (let ((reward (play game choice)))
           (format t "Reward: ~A~%" reward)
           (format t "Average Reward: ~A~%"
                   (/ (total-reward game) (n-played game)))))
        (t
         (format t "Invalid choice. Please try again.~%"))))
  (format t "Game has ended~%")
  (when (> (n-played game) 0)
    (format t "Total Reward: ~A~%" (total-reward game))
    (format t "Average Reward: ~A~%" (/ (total-reward game) (n-played game))))))

(defmethod reset-game ((game gaussian-bandit-game))
  (with-slots (bandits rewards total-reward n-played) game
    (setf bandits (shuffle-list bandits))
    (setf rewards nil)
    (setf total-reward 0)
    (setf n-played 0)))


(defun shuffle-list (list)
  "Shuffle list using Fisher-Yates algorithm"
  (let ((l (copy-list list))
	(n (length list)))
    (loop for i from (1- n) downto 1 do
      (let ((j (random (1+ i))))
	(rotatef (nth i l) (nth j l))))
    l))


;;; Simulate game
(defun run-gaussian-bandit-game ()
  ;; Create a list of bandits
  (let* ((bandits (list (make-bandit :mean 0.5 :std 0.2)
                        (make-bandit :mean 0.6 :std 0.3)
                        (make-bandit :mean 0.4 :std 0.1)))
         ;; Create the game instance
         (game (make-instance 'gaussian-bandit-game :bandits bandits)))
    ;; Start the game
    (user-play game)))
