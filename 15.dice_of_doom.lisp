; global variables
(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))


; player 0 vs player 1
; ((0 3) (0 3) (1 3) (1 1))

(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

; ゲーム盤初期化
(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                 collect (list (random *num-players*) (1+ (random *max-dice*))))))

; 0 1 2 ... -> a b c ...
(defun player-letter (n)
  (code-char (+ 97 n)))

;
(defun draw-board (board)
(loop for y below *board-size*
  do (progn
        (fresh-line)
          (loop repeat (- *board-size* y)
            do (princ "  "))
        (loop for x below *board-size*
          for hex = (aref board (+ x (* *board-size* y)))
          do (format t "~a-~a " (player-letter (first hex)) (second hex))))))


; spare-dice = 現在の手番でいくつサイコロを獲得したか
; 最初の指し手かどうか。最低一回刺さないと、pass などはできないようにする
(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

(defun add-passing-move (board palyer spare-dice first-move moves)
  (if first-move
    moves
    (cons (list nil (game-tree (add-new-dice board player (1- spare-dice))
    (mod (1+ player *num-players*))
      0
      t))
    moves)))

