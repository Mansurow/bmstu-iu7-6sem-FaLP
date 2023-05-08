(setf lst1 '(a b c))
(setf lst2 '(d e))

(cons lstl lst2) ; ((a b c) d e)
(list lst1 lst2) ; ((a b c) (d e))
(append lst1 lst2) ; (a b c d e)

(reverse '(a b c)) ; (c b a)
(reverse '(a b (c (d)))) ; ((c (d)) b a)
(reverse '(a)) ; (a)
(last '(a b c)) ; (c)
(last '(a)) ; (a)
(last '((a b c))) ;((a b c))
(reverse ()) ; Nil
(reverse '((a b c))) ; ((a b c))
(last '(a b (c))) ; ((c))
(last ()) ; Nil

(defun get_last (lst)
    (car (reverse lst))
)

(defun get_last (lst)
    (car (last lst))
)

(defun get_last (lst)
    (
        cond
        (
            (cdr  lst)
            (get_last  (cdr lst))
        )
        (
            T
            (car lst)
        )
    )
)

(defun without_last (lst)
    (
        cond
        (
            (cdr lst)
            (cons (car lst) (without_last (cdr lst)))
        )
        (
            T
            Nil
        )
    )
)

(defun without_last (lst)
    (reverse (cdr (reverse lst)))
)

(defun swap-first-last (lst)
    (append
        (last lst)
        (reverse (cdr (reverse (cdr lst))))
        (cons (car lst) Nil)
    )
)

(defvar dices)
(defvar roll1)
(defvar roll2)

(defun roll_1_dice ()
    (+ (random 6) 1)
)

(defun roll ()
    (+ (roll_1_dice) (roll_1_dice))
)

(defun is_win (throw)
    (cond
        ((or (= throw 7) (= throw 11)) T)
        (T Nil)
    )
)

(defun is_luck (throw)
    (cond
        ((or (= throw 2) (= throw 12)) T)
        (T Nil)
    )
)

(defun turn ()
    (terpri)
    (setq dices (roll))
    (princ "Rolled ")
    (princ dices)

    (cond
        ((is_win dices) 0)
        ((is_luck dices) (turn))
        (T dices)
    )
)

(defun game()
    (princ "First player is rolling: ")
    (setq roll1 (turn))
    (terpri)

    (cond
        ((eq roll1 0) (princ "First player won absolutely!"))
        (T ((lambda ()
                (princ "Second player is rolling: ")
                (setq roll2 (turn))
                (terpri)
                (cond
                    ((eq roll2 0) (princ "Second player won absolutely"))
                    ((eq roll1 roll2) (princ "Draw!"))
                    ((> roll1 roll2) (princ "First player won!"))
                    (T (princ "Second player won!"))
                )

        )))
    )
    (terpri)
)

(defun full_check (lst1 lst2 n)
    (cond
        (
            (> n 1)
            (and
                (= (car lst1) (car lst2))
                (full_check (cdr lst1) (cdr lst2) (- n 1))
            )
        )
        (T T)
    )
)
(defun is_palindrom (lst)
    (full_check lst (reverse lst) (length lst))
)

(defun is_palindrom (lst)
    (equalp lst (reverse lst))
)

(defun show_on_map (lst item)
    (cond
        ((equal (caar lst) item) (cdar lst))
        ((equal (cdar lst) item) (caar lst))
        ((cdr lst) (show_on_map (cdr lst) item))
    )
)

(defun mul_if_all_num (lst num)
    (cond
        ((and
            (numberp num)
            (and
                (numberp (car lst))
                (and
                    (numberp (cadr lst))
                    (numberp (caddr lst))
                )
            )
        ) (* (car lst) num))
        (T Nil)
    )
)

(defun mul_first_num (lst num)
    (cond
        ((and (numberp (car lst)) (numberp num)) (* (car lst) num)))
        ((cdr lst) (mul_first_num (cdr lst) num))
        (T Nil)
    )
)