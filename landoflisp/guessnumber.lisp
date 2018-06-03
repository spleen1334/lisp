;; Globals
(defparameter *small* 1)
(defparameter *big* 100)

;; Functions
(defun guess-my-number ()
  ;; ash - je binary search
  (ash (+ *small* *big*) -1))

(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

;; Reset
(defun start-over ()
  (defparameter *small* 1)
  (defparamter *big* 100)
  (guess-my-number))
