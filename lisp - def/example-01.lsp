(defun make-mtr-zero (n m) (make-array (list n m)))

(setq beta 0.02)
(setq f0 300)

(setq a 10)
(setq b 10)

(defun pow (x n)
  (cond ((zerop n) 1)
        ((< n 0) (* (/ 1 x) (pow x (+ n 1))))
        (t (* x (pow x (- n 1))))))

(defun f (x z) 
    (*
    (exp
    (* 
    (* 
    (* (- x 5) (- x 5))
    (* (- z 5) (- z 5))
    )
    (- 0 beta)
    )
    )
    f0
    )
)

;(defun f (x z) 0)

(setq h 1)
(setq n (floor (/ a h)))
(setq m (floor (/ b h)))

(setq xVect (make-array n))
(setq zVect (make-array m))

(setq uMtr (make-mtr-zero n m))

(loop for i from 0 to (- n 1) do
    (setf (aref xVect i) (* i h))
)
(loop for i from 0 to (- m 1) do
    (setf (aref zVect i) (* i h))
)


; Граничные условия задаю
(dotimes (i n)
    (setf (aref uMtr i 0) 300)
)
(dotimes (i n)
    (setf (aref uMtr i (- n 1)) 300)
)
(dotimes (i m)
    (setf (aref uMtr 0 i) 300)
)
(dotimes (i m)
    (setf (aref uMtr (- m 1) i) 300)
)

; Заполнение матрицы 
; алгоритма Гаусса – Зейделя решения задачи Дирихле

(setq dMax 100)
(setq dm 100)
(setq k 0)
(setq tmp 0)
(setq eps 0.0001)

(loop while (> dMax eps)
    do 
(setq dMax 0)
(loop for i from 1 to (- n 2)
    do 
        (loop for j from 1 to (- m 2)
            do 
                (setq tmp
                    (* 0.25
                    (- (+  (aref uMtr i (+ j 1)) 
                        (aref uMtr i (- j 1)) 
                        (aref uMtr (+ i 1) j) 
                        (aref uMtr (- i 1) j)
                    )
                    (* h (* h (f (aref xVect i) (aref zVect j))))
                    )
                    )
                )
                (setq dm 
                    (abs 
                    (- tmp (aref uMtr i j))
                    )
                )
                (if (< dMax dm)
                    (setq dMax dm)
                )
                (setf (aref uMtr i j) tmp)
        )
)
)


(dotimes (i n) 
        (write (aref xVect i))
        (princ " ")
    )
(terpri)
(dotimes (i n) 
        (write (aref zVect i))
        (princ " ")
    )
(terpri)
(dotimes (i n)
    (dotimes (j m) 
        (write (aref uMtr i j))
        (princ " ")
    )
    (terpri)
)

