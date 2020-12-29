(load "text_game.lisp")

(defun say-hello ()
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))

(defparameter *foo* '(+ 1 2))
(eval *foo*)

(defun simple-game-repl ()
  (loop (print (eval (read)))))

;; (defun game-repl ()
;;   (let ((cmd (game-read)))
;;     (unless (eq (car cmd) 'quit)
;;             (game-print (game-eval cmd))
;;             (game-repl))))

; game-read
; (look) じゃなく look で認識するようにする
; コマンドの引数のシンボルにクォートをつけなくてもいいように: (walk 'east) -> (walk east)
(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x) (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

; game-print
; 大文字と小文字を適切に選択する
(defun tweak-text (lst caps lit)
  (when lst (
    let (
      (item (car lst))
      (rest (cdr lst))
    )
    (cond
      ((eql item #\space) (cons item (tweak-text rest caps lit)))
      ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
      ((eql item #\") (tweak-text rest caps (not lit)))
      (lit (cons item (tweak-text rest nil lit)))
      (caps (cons (char-upcase item) (tweak-text rest nil nil)))
      (t (cons (char-downcase item) (tweak-text rest nil nil)))
    )
  )))


(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
    (prin1-to-string lst))
  'list)
  t
  nil)
'string)
(fresh-line)))

; coerce : value -> ~encode:symbole -> new_value
; string -> char list にして修正してから、char list -> string に戻している
; 最後に princ  で出力
