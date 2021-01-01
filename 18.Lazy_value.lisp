(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nil))
        (lambda ()
          (unless ,forced
            (setf ,value (progn ,@body))
            (setf ,forced t)
          )
          ,value
        ))))

(defun force (lazy-value)
  (funcall lazy-value))

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

(defparameter *integers*
  (labels (
      (f (n) (lazy-cons n (f (1+ n)))))
  (f 1)))


; リストを遅延値にする
; list の hd, tail の部分を再帰的に遅延値にしていく
; (lazy (hd . lazy (hd . lazy (hd . lazy tail...))))
(defun make-lazy (lst)
  (lazy (when lst
    (cons (car lst) (make-lazy (cdr lst))))))

(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car) (take-all (lazy-cdr lst)))))

; find-if
; mapcar
; mapcan
; これらも lazy list バージョンが作れるがめんどいのでやらない
; 遅延リストライブラリを完成させると、ダイスオブドゥームの改善ができる


