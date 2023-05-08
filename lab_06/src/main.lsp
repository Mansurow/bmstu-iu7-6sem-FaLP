(defun my-reverse (lst &optional (res ()))
    (
        cond ((null lst) res)
            (T (my-reverse (cdr lst) (cons (car lst) res)))
    )
)

(defun get-first-lst (lst)
    (
        cond ((null (car lst)) (get-first-lst (cdr lst)))
            (T (car lst))
    )
)

(defun get-first-lst (lst)
    (
        cond ((null lst) nil)
            ((and (listp (car lst)) (not (null (car lst))))
                (car lst))
            (T (get-first-lst (cdr lst)))
    )
)

(defun f_a (lst num &optional (res ()))
    (
        cond ((null lst) nil)
            ((numberp (car lst)) (f_a (cdr lst) num (cons (* (car lst) num) res)))
            ((listp (car lst)) (f_a (cdr lst) num res))
    )
)

(defun f_b (lst num &optional (res ()))
    (
        cond ((null lst) nil)
            (T (f_b (cdr lst) num (cons (* (car lst) num) res)))
    )
)

(defun select-between (lst left right &optional (res ())) (
    cond ((< right left) Nil)
        (T (
            sort (cond ((null lst) nil)
                (T (cond ((and (> (car lst) left) (> right (car lst)))
                    (select-between (cdr lst) left right (cons (car lst) res)))
                    (T (select-between (cdr lst) left right res)))
                )
             lst #'<
        ))
))

(defun rec-add-a (lst &optional (sum 0))
    (
        cond ((null lst) sum)
            ((numberp (car lst)) (rec-add-a (cdr lst) (+ sum (car lst))))
            (T (rec-add-a (cdr lst) sum))
    )
)

(defun rec-add-b (lst &optional (sum 0))
    (
        cond ((null lst) sum)
            ((numberp (car lst)) (rec-add-b (cdr lst) (+ sum (car lst))))
            ((consp (car lst)) (rec-add-b (cdr lst) (rec-add-b (car lst) sum)))
            (T (rec-add-b (cdr lst) sum))
    )
)


(defun get_first_list (lst)
    (cond ((null lst) nil)
        ((listp (car lst)) (car (car lst)))
        (T (get_first_list (cdr lst)))
    )
)

(defun f_a (lst num res)
    (cond ((null lst) res)
        ((numberp (car lst)) (f_a (cdr lst) num (cons (* (car lst) num) res)))
        (T (f_a (cdr lst) num (cons (car lst) res)))
    )
)

(defun f_b (lst num res)
    (cond ((null lst) res)
        (T (f_b (cdr lst) num (cons (* (car lst) num) res)))
    )
)

(defun select-between (lst left right &optional (res ())) (
    cond ((< right left) Nil)
        (T (
            sort (cond ((null lst) nil)
                (T (cond ((and (> (car lst) left) (> right (car lst)))
                    (select-between (cdr lst) left right (cons (car lst) res)))
                    (T (select-between (cdr lst) left right res)))
                )
             lst #'<
        ))
))