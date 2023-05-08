(defun minus-ten (lst) (
    mapcar #'(lambda (el)
        (cond ((numberp el) (- el 10))
            (T el))
        ) lst
    )
)


(defun minus-ten (lst) (
    mapcan #'(lambda (el)
        (cons
            (cond ((numberp el) (- el 10))
                (T el))
            Nil)
        )
        lst)
)

(defun sqr-lst (lst) (
    mapcar #'(lambda (el)
        (* el el)) lst
    )
)

(defun sqr-lst (lst) (
    mapcan #'(lambda (el)
        (cons (* el el) Nil))
        lst)
)

(defun mul-lst (lst num) (
    mapcar #'(lambda (el)
        (* el num)) lst
    )
)

(defun mul-lst (lst num) (
    mapcan #'(lambda (el)
        (cons (* el num) Nil))
        lst)
)

(defun mul-lst (lst num) (
    mapcar #'(lambda (el)
        (cond ((numberp el) (* el num))
            (T el))
        ) lst
    )
)

(defun mul-lst (lst num) (
    mapcan #'(lambda (el)
        (cons
            (cond ((numberp el) (* el num))
                (T el))
            Nil)
        )
        lst)
)

(defun is-palindrom (lst)
    (cond ((eql
        (find-if #'evenp
            (mapcar #'(lambda (elem reverse-elem)
                (cond ((eql elem reverse-elem) 1)
                    (T 0))
            ) lst (reverse lst))
        ) Nil) T)
        (T Nil))
)

(defun set-equal (set1 set2) (
    and (eq (length set1) (length set2))
        (every #'(lambda (el)
            (member el set2 :test #'equal)
        ) set1)
        (every #'(lambda (el)
            (member el set1 :test #'equal)
        ) set2)
))

(defun select-between (lst left right) (
    cond ((< right left) Nil)
        (T (
            sort (reduce #'(lambda (result el)
                (cond ((and (> el left) (> right el))
                            (cons el result))
                        (T result)
                )
            ) lst :initial-value ()) #'<
        ))
))

(defun decart (lst1 lst2) (
    mapcan #'(lambda (x)
        (mapcar #'(lambda (y)
            (list x y)) lst2)) lst1
))

(reduce #'+ ()) ; -> 0
(reduce #'* ()) ; -> 1

(defun len-list-of-lists (lst) (
    apply #'+ (
        mapcar #'(lambda (el)
            (cond ((listp el) (len-list-of-lists el))
                (T 1)
            )
        ) lst
    )
))