'(1 . (2 . (3 . nil)))

(defparameter foo '(1 2 3))
(setf (cdddr foo) foo)

(defparameter *drink-order* '(
  (bill . double-espresso)
  (lisa . small-drip-coffee)
  (john . medium-latte)))

(assoc 'bill *drink-order*)
(assoc 'lisa *drink-order*)


(defparameter *house* '(
  (walls
    (mortar (cement) (water) (sand))
    (bricks))
  (windows
    (glass)
    (frame)
    (curtains))
  (roof
    (shingles)
    (chimney))
))


(defparameter *wizard-nodes* '(
  (living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
  (garden (you are in a beautiful garden. there is a well in front of you.))
  (attic (you are in the attic. there is a giant welding torch in the corner.))
  )
)
(defparameter *wizard-edges* '(
  (living-room (garden west door) (attic upstairs ladder))
  (garden (living-room east door))
  (attic (living-room downstairs ladder))
))

; .dot ファイルはノードの名前に 半角英数字と _ しか使えないので、 ! とか ? は _ に変換する

; substitute-if : value -> value list -> (string | list) -> (string | list)
; 第三引数の list or string にある要素のうち、第二引数に含まれているものを第一引数で置き換える
; ジェネリックな関数であり、 string でも list でもできる。

; substitute-if-not もあるが、not のついている関数は非推奨で将来的に取り除かれるかも

; complement : (proc : value -> bool) -> list
; bool を返す関数を渡すと、false になる値の list を返す。つまり、補集合を求めるための関数


(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp))
)

(defparameter *max-label-length* 30)
(defun dot-label (exp)
  (if exp
    (let ((s (write-to-string exp :pretty nil)))
      (if (> (length s) *max-label-length*)
        (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
        s))
    "")
)

(defun nodes->dot (nodes)
  (mapc (lambda (node)
      (fresh-line)
      (princ (dot-name (car node)))
      (princ "[label=\"")
      (princ (dot-label node))
      (princ "\"];"))
  nodes
  ))

(defun edges->dot (edges)
  (mapc (lambda (node)
    (mapc (lambda (edge)
      (fresh-line)
      (princ (dot-name (car node)))
      (princ "->")
      (princ (dot-name (car edge)))
      (princ "[label=\"")
      (princ (dot-label (cdr edge)))
      (princ "\"];"))
      (cdr node)))
    edges)
)

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}")
)

(defun dot->png (fname thunk)
  (with-open-file (
    *standard-output*
    (concatenate 'string fname ".dot")
    :direction :output
    :if-exists :supersede
  )
  (funcall thunk))
  (ext:shell (concatenate 'string "neato -Tpng -O " fname))
)

(defun graph->png (fname nodes edges)
  (dot->png fname
    (lambda () (graph->dot nodes edges))
  )
)

(defun uedges->dot (edges)
  (maplist (lambda (lst)
    (mapc (lambda (edge)
      (unless (assoc (car edge) (cdr lst))
        (fresh-line)
        (princ (dot-name (caar lst)))
        (princ "--")
        (princ (dot-name (car edge)))
        (princ "[label=\"")
        (princ (dot-label (cdr edge)))
        (princ "\"];")))
        (cdar lst)))
      edges))
(defun ugraph->dot (nodes edges) (princ "graph{") (nodes->dot nodes) (uedges->dot edges) (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
  (lambda ()
    (ugraph->dot nodes edges))))
