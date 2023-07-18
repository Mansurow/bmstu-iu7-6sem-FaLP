; ======================================================================================================
(with-open-file (str "log.txt"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (format str "============== Start ==========================~%")
)

(defun fprint-next-layer (name A B C D xi eta y)
    (with-open-file (str "log.txt"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
        (format str "------------------ NextLayer~S  -----------------------~%" name)
        (format str "A: ")
        (fprint-list str A)
        (format str "~%")
        (format str "C: ")
        (fprint-list str C)
        (format str "~%")
        (format str "B: ")
        (fprint-list str B)
        (format str "~%")
        (format str "D: ")
        (fprint-list str D)
        (format str "~%")
        (format str "xi: ")
        (fprint-list str xi)
        (format str "~%")
        (format str "eta: ")
        (fprint-list str eta)
        (format str "~%")
        (format str "eta: ")
        (fprint-list str y)
        (format str "~%-----------------------------------------------------~%")
    ) 
)

(defun fprint-next-time (midy newy)
    (with-open-file (str "log.txt"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
        (format str "------------------ Next Time -----------------------")
        (format str "~%midy (данные прогонка по Z)")
        (fprint-dlist str newy)
        (format str "~%newy (данные прогонка по X):~%")
        (fprint-dlist str newy)
        (format str "~%-----------------------------------------------------~%")
    ) 
)

(defun print-list (lst)
  (when lst
    (princ (car lst))
    (princ " ")
    (print-list (cdr lst))
   )
)

(defun print-double-list (dlst)
    (when dlst
        (print-list (car dlst))
        (terpri)
        (print-double-list (cdr dlst)) 
    )
)

(defun fprint-list (str lst)
    (if (null lst)
        nil
        (or 
            (format str "~A " (car lst)) 
            (fprint-list str (cdr lst))
        )
    )
)

(defun fprint-dlist (str dlst)
    (if (null dlst)
        nil
        (or
            (fprint-list str (car dlst))
            (format str "~%")
            (fprint-dlist str (cdr dlst))
        )
    )
)

(defun print-result (x z y n m itr)
    (terpri)
    (princ "Количество итераций: ")
    (princ itr)
    (terpri)
    (princ "Размеры: ")
    (princ n)
    (princ " ")
    (princ m)
    (terpri)
    (princ "Список значений по оси OX: ")
    (print-list x)
    (terpri)
    (princ "Список значений по оси OZ: ")
    (print-list z)
    (terpri)
    (princ "Список значений по оси OY: ")
    (terpri)
    (print-double-list y)
)

(defun fprint-result (filename x z y n m itr flag)
    (if (= flag 0)
        (print-result x z y n m itr)
        (print "Finish")
    )

    (with-open-file (str filename
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
        (format str "~A ~A~%" n m)
        (fprint-list str x)
        (format str "~%")
        (fprint-list str z)
        (format str "~%")
        (fprint-dlist str y)
    )    
)
;=======================================================================================================

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

(defun my+ (&rest args)
    (reduce #'+ args)
)

(defun my- (&rest args)
    (reduce #'- args)
)

(defun my* (&rest args)
    (reduce #'* args)
)

(defun my/ (&rest args)
    (reduce #'/ args)
)
;----------------------------------- Тестирование ------------------------------------------------

;(print (my* 1 2 3 4))

;=================================================================================================
; Глобальные значения для продольно-попречной прогонки решая уравнения теплопровдности Э)
(defvar beta 0.5)
(defvar f0 30)
(defvar xmax 50.0)
(defvar zmax 50.0)
(defvar n 100)
(defvar m 100)
(defvar hx (/ xmax n))
(defvar hz (/ zmax n))
(defvar x0 (/ xmax 2))
(defvar z0 (/ zmax 2))
(defvar x1 (/ xmax 6))
(defvar z1 (/ zmax 6))
(defvar x2 (/ xmax 4))
(defvar z2 (/ zmax 4))
(defvar FB0 0)
(defvar T0 300) 
(defvar tau 1)
(defvar eps 0.00001)

; (defun F (x z)
;     0
; )

; (defun f (x z)
;     (* f0 (exp (* (- 0 beta)
;                   (- x x0)
;                   (- x x0)
;                   (- z z0)
;                   (- z z0)
;                )
;           )
;     )
; )

(defun f (x z)
    (my* f0 (exp (my* (my- 0 beta)
                    (my+ (my* (my- x x0)
                    (my- x x0))
                    (my* (my- z z0)
                    (my- z z0))) 
                )
            )
    )
)

(defun create-list-pr (n i f)
    (if (zerop n)
        nil
        (if (zerop i)
            (cons (apply #'f '()) (create-list-pr (1- n) (1+ i) f))
            (cons 0 (create-list-pr (1- n) (1+ i) f))
        )
    )
)

; ================================================================ Прогонка =========================================================

; --- Коэффициенты

(defun my-lambda (temp)
    1.5
)

(defun calc-An (temp)
    (/ (my-lambda temp) (* hx hx))
)

(defun calc-Cn (temp)
    (/ (my-lambda temp) (* hx hx))
)

(defun calc-Bn (temp)
    (/ (+ (calc-An temp) (calc-Cn temp) 2) tau)
)

; ynm_1
; ynm
; ynm1
; x
; z
(defun calc-Dn (ynm_1 ynm ynm1 x z)
    (let (
            (lnm12 (/ (+ (my-lambda ynm1) (my-lambda ynm)) 2))
         )
        (+ (* 2 (/ ynm tau)) 
           (*   lnm12
                (/ (+ (- ynm_1 (* 2 ynm)) ynm1) 
                   (* hz hz)
                )
           )
           (F x z)
        )
    )
)

(defun calc-Am (temp)
    (/ (my-lambda temp) (* hz hz))
)

(defun calc-Cm (temp)
    (/ (my-lambda temp) (* hz hz))
)

(defun calc-Bm (temp)
    (/ (+ (calc-Am temp) (calc-Cm temp) 2) tau)
)

(defun calc-Dm (yn_1m ynm yn1m x z)
    ; (print "--------------------------------------")
    ; (print x)
    ; (print z)
    ; (print (/ (+ (my-lambda yn1m) (my-lambda ynm)) 2))
    ; (print "-------------------------------------")
    (let* (
            (ln12m (/ (+ (my-lambda yn1m) (my-lambda ynm)) 2))
            (res (+ (* 2 (/ ynm tau)) 
                        (*   ln12m
                                (/ (+ (- yn_1m (* 2 ynm)) yn1m) 
                                (* hx hx)
                                )
                        )
                        (F x z)
                        ))
        )
        res
    )
)

;(print (calc-Dm 300 300 300 1 1))

(defun make-list-A (n A)
    (nconc (cons 0
                (map 'list (lambda (x) (apply A '(1))) (make-list (- n 2)))
            ) '(0) 
    )
)

(defun make-list-B (n B)
    (nconc (cons 0
                (map 'list (lambda (x) (apply B '(1))) (make-list (- n 2))) 
            ) '(0) 
    )
)

(defun make-list-C (n C) 
    (nconc (cons 0
                (map 'list (lambda (x) (apply C '(1))) (make-list (- n 2))) 
            ) '(0) 
    )
)

(defun make-list-D (n ynm_1 ynm ynm1 x z D)
    (nconc (cons 0
                (map 'list (lambda (_ vynm_1 vynm vynm1 x z) (apply D (list vynm_1 vynm vynm1 x z))) (make-list (- n 2)) (cdr ynm_1) (cdr ynm) (cdr ynm1) x z) 
            ) '(0)
    )
)

;--------------------------------- Тестирование вычисление коэффициентов ----------------------------------
; (print (make-list-An 7))
; (print (make-list-Bn 7))
; (print (make-list-Cn 7))
; (print (make-list-D 7
;                     '(300 300 300 300 300 300 300)
;                     '(300 300 300 300 300 300 300)
;                     '(300 300 300 300 300 300 300)
;                     '(0 1.4285 2.8571 4.2857 5.7143 7.1428 8.5714)
;                     '(1.4285 1.4285 1.4285 1.4285 1.4285 1.4285 1.4285)
;                     #'calc-Dn
;                     ))
; (print (make-list-coeffs 7))

; Test Dn calculate
; (print (calc-Dn 300 300 300 1.4285 1.4285))
; (print (calc-Dn 300 300 300 2.8571 1.4285))
; (print (calc-Dn 300 300 300 4.8571 1.4285))

; ----------------------------------------------------------------------------------------------------------
; a - значение A-коэффицентa - a[i]
; b - значение B-коэффицентa - b[i]
; c - значение C-коэффицентa - c[i]
; xi - значение xi - xi[i-1]
(defun calc-xi (a b c xi)
    (/ c (- b (* a xi)))
)

; a - значение A-коэффицентa - a[i]
; b - значение B-коэффицентa - b[i]
; c - значение C-коэффицентa - c[i]
; xi - значение xi - xi[i-1]
; eta - значение eta - eta[i-1]
(defun calc-eta (a b c d xi eta)
    (/ (+ (* a eta) d) (- b (* a xi)))
)

; xi - значение xi - xi[i-1]
; eta - значение eta - eta[i-1]
; y - значение y - y[i]
(defun calc-y (xi eta y)
    (+ (* xi y) eta)
)

; for (int i = 1; i < xi.Length; i++)
; a - список значений A-коэффицентов
; b - список значений B-коэффицентов
; c - список значений C-коэффицентов
; xi - xi[0] - начальное значение
; fl - флаг
; n - размер xi (размер списка коэф - 1)
(defun calc-xi-list (a b c xi fl n)
    (if (zerop n)
        nil
        (if (zerop fl)
            (cons xi (calc-xi-list a b c xi 1 (1- n)))
            (cons   (calc-xi (car a) (car b) (car c) xi)
                    (calc-xi-list (cdr a) (cdr b) (cdr c) (calc-xi (car a) (car b) (car c) xi) 1 (1- n))
            )
        )
    )
)

; a - список значений A-коэффицентов
; b - список значений B-коэффицентов
; c - список значений C-коэффицентов
; d - список значений D-коэффицентов
; xi - список значений xi
; eta - eta[0] - начальное значение
; fl - флаг
; n - размер eta (размер списка коэф - 1)
(defun calc-eta-list (a b c d xi eta fl n)
    (if (zerop n)
        nil
        (if (zerop fl)
            (cons eta (calc-eta-list a b c d xi eta 1 (1- n)))
            (cons   (calc-eta (car a) (car b) (car c) (car d) (car xi) eta)
                    (calc-eta-list (cdr a) (cdr b) (cdr c) (cdr d) (cdr xi) (calc-eta (car a) (car b) (car c) (car d) (car xi) eta) 1 (1- n))
            )
        )
    )
)

; n - размер y (размер списка коэф)
(defun calc-y-list (xi eta y fl n)
    ;(print y)
    (if (zerop n)
        nil
        (if (zerop fl)
            (cons y (calc-y-list xi eta y 1 (1- n)))
            (cons   (calc-y (car xi) (car eta) y)
                    (calc-y-list (cdr xi) (cdr eta) (calc-y (car xi) (car eta) y) 1 (1- n))
            )
        )
    )
)

(defun get-y-list (xi eta y n)
    (reverse (calc-y-list xi eta y 0 n))
)

; -------------------------------------------------- Тестирование прогонки ----------------------------------------------------------------
; (print (calc-xi-list '(150.0 150.0 150.0 150.0 150.0 150.0 0) '(302 302 302 302 302 302 0) '(150 150 150 150 150 150 0) 1 0 6))
; ; (1 0.9868421 0.9741925 0.9623335 0.9514749 0.94174516 0.93319434)
; (print (calc-eta-list '(150.0 150.0 150.0 150.0 150.0 150.0 0) 
;                      '(302 302 302 302 302 302 0) 
;                      '(150 150 150 150 150 150 0) 
;                      '(600 600 600 600 600 600 0) 
;                      (calc-xi-list '(150.0 150.0 150.0 150.0 150.0 150.0 0) '(302 302 302 302 302 302 0) '(150 150 150 150 150 150 0) 1 0 6)
;                      (/ (* 30 0.1) 10) 0 6))
; ; (0.3 4.243421 8.030679 11.577525 14.821626 17.725174 20.27381)

; (print (reverse '(1 0.9868421 0.9741925 0.9623335 0.9514749 0.94174516 0.93319434)))
; (print (calc-y-list (reverse '(1 0.9868421 0.9741925 0.9623335 0.9514749 0.94174516 0.93319434))
;                     (reverse '(0.3 4.243421 8.030679 11.577525 14.821626 17.725174 20.27381))
;                     300 0 7))
; (print "Tecт 2 - прогонка")
; (terpri)
; ; Тест 2
; (let ((b '(0 3.47 3.47 3.47 3.47 3.47 0))
;       (a '(0 0.735 0.735 0.735 0.735 0.735 0))
;       (c '(0 0.735 0.735 0.735 0.735 0.735 0))
;       (d '(0 600 600 611.58 611.58 600 0))
;       (xi '(1 0.26873857 0.22460051 0.2223958 0.2222868 0.22228143))
;       (eta '(4.285 220.52997 232.8785 236.84265 237.60777 234.27002))
;       (y '(305.7815 301.4965 301.28363 304.5635 304.50598 300.95444 300))
;      )
;      (princ "xi = ")
;      (princ (calc-xi-list (cdr a) (cdr b) (cdr c) 1 0 6))
;      (terpri)
;      (princ "eta = ")
;      (princ (calc-eta-list (cdr a) (cdr b) (cdr c) (cdr d) xi 4.285 0 6))
;      (terpri)
;      (princ "y = ")
;      (princ (get-y-list (reverse xi) (reverse eta) 300 7))
;      (terpri)
; )                                        

; (let ((b '(0 3.47 3.47 3.47 3.47 3.47 0))
;       (a '(0 0.735 0.735 0.735 0.735 0.735 0))
;       (c '(0 0.735 0.735 0.735 0.735 0.735 0))
;       (d '(0 600.0 600.0 600.0 600.0 600.0 0))
;       (xi '(1 0.26873857 0.22460051 0.2223958 0.2222868 0.22228143))
;       (eta '(4.2857146 220.53017 232.87852 233.33878 233.32675 233.31842) )
;       (y '(305.7815 301.4965 301.28363 304.5635 304.50598 300.95444 300))
;      )
;      (print "Test 3")
;      (terpri)
;      (princ "xi = ")
;      (princ (calc-xi-list (cdr a) (cdr b) (cdr c) 1 0 6))
;      (terpri)
;      (princ "eta = ")
;      (princ (calc-eta-list (cdr a) (cdr b) (cdr c) (cdr d) xi 4.285 0 6))
;      (terpri)
;      (princ "y = ")
;      (princ (get-y-list (reverse xi) (reverse eta) 300 7))
;      (terpri)
; )    

; =============================== Next Layer (подготовка к прогонке) ========================================================

; начальные коэффициенты прогонки
(defun xiStartX ()
    0
)

(defun etaStartX ()
    T0 ;(* 30 (/ hx 15))
)

(defun yEndX (eta xi)
    T0 ;(/ eta (- 1 xi))
)

(defun NextLayerX (_ ym_1 ym ym1 x z)
    (let* (
            (n (length ym))
            (A (make-list-A n #'calc-An))
            (C (make-list-C n #'calc-Cn))
            (B (make-list-B n #'calc-Bn))
            (D (make-list-D n ym_1 ym ym1 x z #'calc-Dn))
            (xi (calc-xi-list (cdr A) (cdr B) (cdr C) (xiStartX) 0 (1- n)))
            (eta (calc-eta-list (cdr A) (cdr B) (cdr C) (cdr D) xi (etaStartX) 0 (1- n)))
          )
        (fprint-next-layer "X" A B C D xi eta (get-y-list (reverse xi) (reverse eta) (yEndZ (car (last eta)) (car (last xi))) n))
        ; (terpri)
        ; (print "New NextLayerX")
        ; (print A)
        ; (print B)
        ; (print C)
        ; (print D)
        ; (print xi)
        ; (print eta)  
        (get-y-list (reverse xi) (reverse eta) (yEndZ (car (last eta)) (car (last xi))) n)
    )
)

; начальные коэффициенты прогонки
(defun xiStartZ ()
    0
)

(defun etaStartZ ()
    T0
)

(defun yEndZ (eta xi)
    T0 ;(/ eta (- 1 xi))
)

(defun NextLayerZ (_ ym_1 ym ym1 x z)
    (let* (
            (m (length ym))
            (A (make-list-A m #'calc-Am))
            (C (make-list-C m #'calc-Cm))
            (B (make-list-B m #'calc-Bm))
            (D (make-list-D m ym_1 ym ym1 x z #'calc-Dm))
            (xi (calc-xi-list (cdr A) (cdr B) (cdr C) (xiStartZ) 0 (1- m)))
            (eta (calc-eta-list (cdr A) (cdr B) (cdr C) (cdr D) xi (etaStartZ) 0 (1- m)))
          )
        (fprint-next-layer "Z" A B C D xi eta (get-y-list (reverse xi) (reverse eta) (yEndZ (car (last eta)) (car (last xi))) n))  
        ; (terpri)
        ; (print "New NextLayerZ")
        ; (print A)
        ; (print B)
        ; (print C)
        ; (print D)
        ; (print xi)
        ; (print eta)
        (get-y-list (reverse xi) (reverse eta) (yEndZ (car (last eta)) (car (last xi))) m)
    )
)
; ------------------------------------ Тестирование Next Layer ----------------------------------------------------------------
; (print (NextLayerX 0 '(300 300 300 300 300 300 300)
;                     '(300 300 300 300 300 300 300)
;                     '(300 300 300 300 300 300 300)
;                     '(0 1.4285 2.8571 4.2857 5.7143 7.1428 8.5714)
;                     '(1.4285 1.4285 1.4285 1.4285 1.4285 1.4285 1.4285)))

; (print (NextLayerZ 0 '(301.2 301.2 301.2 301.2 301.2 301.2 301.2)
;                      '(305.5 305.5 305.5 305.5 305.5 305.5 305.5)
;                      '(300.37 300.37 300.3 300.37 300.37 300.37 300.37)
;                      '(1.4285 1.4285 1.4285 1.4285 1.4285 1.4285 1.4285)
;                      '(0 1.4285 2.8571 4.2857 5.7143 7.1428 8.5714)
;                      ))

;------------------------- Next Time - Нахождение промежуточного слоя --------------------------------------------------------

(defun Double2List (val n)
    (if (zerop n)
        nil
        (cons val (Double2List val (1- n)))
    )
)

(defun transp (matr)
  (apply 'mapcar (cons 'list matr))) 

(defun NextTime (x z y_)
    (let* (
            (temp-midy (map 'list 'NextLayerX (make-list (length y_)) 
                            y_ 
                            (cdr y_) 
                            (cddr y_) 
                            (Double2List x (length x))
                            (map 'list 'Double2List z 
                                                    (map 'list (lambda (_) (length z)) (make-list (length z)))
                            )
                        )
            )
            (midy (transp (append (cons (car temp-midy) nil) temp-midy (last temp-midy))))
            (temp-newy (map 'list 'NextLayerZ (make-list (length y_))
                                              midy ; midy[i - 1]
                                              (cdr midy) ; midy[i]
                                              (cddr midy) ; midy[i + 1]
                                              (map 'list 'Double2List x 
                                                    (map 'list (lambda (_) (length x)) (make-list (length x)))
                                              )
                                              (Double2List z (length z))
                       )
            )
            (newy (transp (append (cons (car temp-newy) nil) temp-newy (last temp-newy))))
        )
        (fprint-next-time midy newy)
        ; (terpri)
        ; (print "New Next Time")
        ; (print midy)
        ; (print newy)
        newy
    )
)

; (print (NextTime '(0 1.4285 2.8571 4.2857 5.7143 7.1428 8.5714)
;                  '(0 1.4285 2.8571 4.2857 5.7143 7.1428 8.5714)
;                  '(
;                    (300 300 300 300 300 300 300)
;                    (300 300 300 300 300 300 300)
;                    (300 300 300 300 300 300 300)
;                    (300 300 300 300 300 300 300)
;                    (300 300 300 300 300 300 300)
;                    (300 300 300 300 300 300 300)
;                    (300 300 300 300 300 300 300)
;                 )
;         )
; )
; --------------------------------------------------------------------------------
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
        (cons (reduce #'max (calc-err-row (car newy)
                                          (car y)
                            )
              )
              (calc-materr (cdr newy)
                           (cdr y)
              )
        )
    )
)

(defun cacl-maxerr (newy y)
    (reduce #'max (calc-materr newy y))
)

;------------------------------ Тестирование нахождение максимальной ошибки -------------------------------
; (print (cacl-maxerr '((302.4498 302.4498 300.54453 300.12103 300.0268 300.00568 300.00568)
;                       (302.4498 302.4498 300.54453 300.12103 300.0268 300.00568 300.00568)
;                       (302.44983 302.44983 300.54456 300.12103 300.02682 300.00568 300.00568)
;                       (302.44983 302.44983 300.54453 300.12103 300.02682 300.00568 300.00568)
;                       (302.44983 302.44983 300.54453 300.12103 300.02682 300.00568 300.00568)
;                       (302.44983 302.44983 300.54456 300.12103 300.02686 300.00568 300.00568)
;                       (302.44986 302.44986 300.54456 300.12103 300.02686 300.0057 300.0057))
;                     '((300 300 300 300 300 300 300)
;                       (300 300 300 300 300 300 300)
;                       (300 300 300 300 300 300 300)
;                       (300 300 300 300 300 300 300)
;                       (300 300 300 300 300 300 300)
;                       (300 300 300 300 300 300 300)
;                       (300 300 300 300 300 300 300)
;                      )
;         )
; )

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

(defun create-list (h n i)
    (if (zerop n)
        '()
        (cons (* i h) (create-list h (1- n) (1+ i)))
    )
)

;(print (create-2d-list 10 10))

;---------------------------------- Запуск main функции ------------------------------------------------
(defun main-itr (data maxerr x z y itr)
    ;(print itr)
    (let (
            (newy (NextTime x z y))
        )
        (if (and (> maxerr 1e-5) T)
            (main-itr data (cacl-maxerr newy y) x z newy (1+ itr))
            (fprint-result data x z newy n m itr 0)    
        )
    )
)

(main-itr  "data.txt"               ; данные для graph.py
            1                       ; maxerr init 
            (create-list hx n 0)    ; список значений x
            (create-list hz m 0)    ; список значений z
            (create-2d-list n m)    ; двойной список (матрица) значений (T0) y
            1                       ; счетчик количество итераций 
)
; ------------------------------------------------------------------------------------------------------
