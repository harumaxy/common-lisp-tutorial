(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)


(defparameter *monsters* nil) ; struct<T> list
(defparameter *monster-builders* nil) ; function list
(defparameter *monster-num* 12)


(defun orc_battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over"))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes."))
)

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
      (lambda (m) (or (monster-dead m) (monster-attack m)))
      *monsters*
    (game-loop)
  )))

(defun init-plauer ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ") (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster) (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
        (princ "Your double swing has a strength of ")
        (princ x)
        (fresh-line)
        (monster-hit (pick-monster) x) (unless (monsters-dead)
        (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))))
  )
)

; wip
