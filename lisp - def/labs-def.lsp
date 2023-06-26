(defun my-sum (lst)
  (if (null lst)
      0
      (+ (car lst) (my-sum (cdr lst)))
  )
)

; сложение двух элементов без системной функции +
(defun my-plus (x y)
    (if (zerop y)
        x 
        (if (> y 0)
            (my-plus (1+ x) (1- y))
            (my-plus (1- x) (1+ y))
        )
    )
)

; разность двух элементов без системной функции -
(defun my-minus (x y)
    (if (zerop y)
        x 
        (if (> y 0)
            (my-minus  (1- x) (1- y))
            (my-minus  (1+ x) (1+ y))
        )
    )
)

; умножение двух элементов без системной функции *
(defun my-multiply (x y)
    (if (zerop y)
        0
        (if (> y 0)
            (my-plus x 
                    (my-multiply x (1- y)))
            (my-minus (my-multiply x (1+ y)) x)
        )
    )
)

; деление двух элементов без использования /
(defun my-divide (x y)
    (if (zerop y)
        (error "Division by zero")
        (if (zerop x)
            0
            (1+ x)
        )
    )
)


; (defun my-plus-args (&rest arg)
;     (if (null args)
;         0
;         ; (if (null (cdr args))
;         ;     (car args)
;         ;     (my-plus (car args) (cdr args))
;         ; )
;         1
;     )
; )

; (print (my-plus 1 2.5))
; (print (my-minus 1 -2))
; ;(print (my-multiply -5 -6))
; (print (apply '+ (cdr '(1 2 3))))

; -------------------------------------------------------------------------------------------------

(defvar beta 1)
(defvar f0 300)
(defvar xmax 10.0)
(defvar zmax 10.0)
(defvar n 100)
(defvar m 100)
(defvar hx (/ xmax n))
(defvar hz (/ zmax n))
(defvar x0 5)
(defvar z0 5)
(defvar F0 0)
(defvar T0 300) 
(defvar tau 1)
(defvar eps 0.00001)

(defun create-list (h n i)
    (if (zerop n)
        '()
        (cons (* i h) (create-list h (1- n) (1+ i)))
    )
)

(defun create-list-T0 (n)
    (if (zerop n)
        '()
        (cons T0 (create-list-T0 (1- n)))
    )
)

(defun create-2d-list (n m)
    (if (zerop n)
        '()
        (cons (create-list-T0 m) (create-2d-list (1- n) m))
    )
)
 
(defun my_lambda (temp)
    1.5
)

(defun F (x z)
    0
)

(defun An ()
    (/ (apply #'my_lambda '(1)) (* hx hx))
)

(defun Cn ()
    (/ (apply #'my_lambda '(1)) (* hx hx))
)

(defun Bn ()
    (/  (+ (apply #'An '())
           (apply #'Cn '())
           2
        )
        tau
    )
)

; есть баг
(defun Dn (ynm_1 ynm ynm1 x z)
    (+  (/  (* 2 ynm)
            (+ tau
               (* (/ (+ (apply #'my_lambda '(ynm1))
                        (apply #'my_lambda '(ynm))
                     )
                     2
                  )
                  (+ ynm_1 (- 0 (* 2 ynm)) ynm1)
               ) 
            )
            (* hz hz)
        )
        (apply #'F '(x z))
    )
)

(defun run_method(a b c d)
    1
)

; mtr - матрицы - двойной список
(defun max-mtr-row (mtr)
    (if (null mtr)
        nil
        (cons (reduce #'max (car mtr))
              (max-mtr-row (cdr mtr))
        )
    )
)

(defun max-mtr (mtr)
    (if (null mtr)
        0
        (reduce #'max (max-mtr-row mtr))
    )
) 

; -----------------------------------------------------------------------------------------
; Вычисление промежуточного слоя
; x - список значений x
; z - список значений z
; y - двумерный список для температуры 

(defun double2list (val n)
    (if (zerop n)
        nil
        (cons val (double2list val (1- n)))
    )
)

(defun list-add-f (n f)
    (if (zerop n)
        nil
        (cons (funcall f) (list-add-f (1- n) f))
    )
) 


#| (defun NextLayer (ym ym_1 ym1 x z)
    (let (
            (a (list-add-f (length ym) #'An))
            (b (list-add-f (length ym) #'Bn))
            (c (list-add-f (length ym) #'Cn))
            (d ())
         )
         ()
    )
) |#

(defun calc-xi (a b c d xi_prev)
    (/ c (- b (* a xi_prev)))
)

(defun calc-eta (a b c d eta_prev xi_prev)
    
)

(defun xi-next (a b c d xi_prev)
    (if (zerop (- (length b) 1))
        nil
        (cons )
    )
)

(defun xi (a b c d)
    (cons 0 (xi-next ))
)



(defun next-time (x z y)
    ; возвращать newy
    '((1 2 3)
     (4 25 6)
     (7 8 9))
)
; -----------------------------------------------------------------------------------------
; Вычисление масимальной ошибки между новым y и y из прошлого временого слоя 
(defun calc-err (newy y)
    (if (zerop newy)
        (error "Division by zero - newy[i][j] is 0")
        (abs (/ (- newy y) newy))
    )
)

(defun calc-err-row (newy y)
    (if (or (null newy) (null y))
        nil
        (cons (calc-err (car newy)
                        (car y)
              )
              (calc-err-row (cdr newy)
                            (cdr y)
              )
        )
    )
)

; y и newy - двумерные списки (матрицы)
(defun calc-materr (newy y)
    (if (or (null newy) (null y))
        nil
        (cons (calc-err-row (car newy)
                            (car y)
              )
              (calc-materr (cdr newy)
                           (cdr y)
              )
        )
    )
)
; ---------------------------------------------------------------------------------------

; x - список значений x
; z - список значений z
; y - двумерный список для температуры 
(defun maxerr-eps (x z y)
    (let ((newy (next-time x z y)))
         (if (> (max-mtr (calc-materr newy y)) eps)
             (calc-materr x z newy)
             (print newy)   
         )
    )      
)

(defun main ()
    0
)

;(print (create-list hx n 0))
;(print (create-list hz m 0))

;(print (create-2d-list n m))

#| (print (max-mtr '((1 2 3)
                (4 25 6)
                (7 8 9)) ))

(maxerr-eps '() '() '((1 2 3)
                (4 25 6)
                (7 8 9)))   |#     

;(print (list-add-f 10 #'An))                        