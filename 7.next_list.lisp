'(1 . (2 . (3 . nil)))

(defparameter foo '(1 2 3))
(setf (cdddr foo) foo)
