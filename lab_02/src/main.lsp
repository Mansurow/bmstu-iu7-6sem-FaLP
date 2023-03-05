; Задание 2. Вычисление гипотенизу по двум катетам
(defun hyp (a b)
  (sqrt (+ (* a a) (* b b)))
)

(defun hyp-02 (a b)
  (if (numberp a)
    (if (numberp b)
      (sqrt (+ (* a a) (* b b)))
    )
  nil)
)

(print (hyp 7 8))
(print (hyp-02 7 8))

; Задание 3. 

(list 'a c) ; variable C has no value
(cons 'a 'b 'c) ; too many arguments given to CONS: (CONS 'A 'B 'C)
(cons 'a (b c)) ; undefined function B
(list 'a (b c)) ; undefined function B
(cons 'a '(b c)) ; (A B C)
(list a '(b c)) ; variable A has no value
(caddr (1 2 3 4 5)) ;  is not a function name; try using a symbol instead
(list (+ 1 '(length '(1 2 3)))) ; (LENGTH '(1 2 3)) is not a numbe

; Задание 4. Написать функцию longer_then от двух списков-аргументов, которая возвращает Т, если первый аргумент имеет большую длину

(defun longer_then (list1 list2)
 (> (length list1) (length list2))
)

(defun longer_then (list1 list2)
  (if (consp list1) 
    (if (consp list2)
        (> (length list1) (length list2))
    t)
  nil)
)

(print (longer_then '(1 2 3 3) '(1 2 3)))
(print (longer_then '(1 2) '(1 2 3)))

; Задание 5.

(cons 3 (list 5 6)) ; (3 5 6) 
(cons 3 '(list 5 6)) ; (3 LIST 5 6) 
(list 3 'from 9 'lives (- 9 3)) ; (3 FROM 9 LIVES 6) 
(+ (length for 2 too) (car '(21 22 23))) ; variable FOR has no value
(cdr '(cons is short for ans)) ; (IS SHORT FOR ANS) 
(car (list one two)) ; variable ONE has no value
(car (list 'one 'two)) ; ONE

; Задание 6.
(defun mystery (x)
  (list (second x) (first x))
)

;(print (mystery (one two))) ; undefined function ONE / variable one is unbound
(print (mystery '(one two))) ; (TWO ONE)
; (print (mystery (last one two))) ;variable ONE has no value / variable one is unbound
(print (mystery (last '(one two)))) ; (NIL TWO) 
; (print (mystery free)) ; variable FREE has no value / variable free is unbound
; (print (mystery 'free)) ; FREE is not a list
; (print (mystery one 'two)) ; variable ONE has no value
; (print (mystery 'one 'two)) ; too many arguments given to MYSTERY

; Задание 7. 
(defun f-to-c (f)
  (* 5/9 (- f 32.0))
)

(defun c-to-f (c)
  (+ (* 9/5 c) 32.0)
)

(print (f-to-c 451))
(print (c-to-f (f-to-c 451)))

; Задание 8. 

(list 'cons t NIL) ; (CONS T NIL) 
(eval (list 'cons t NIL)) ; (T)
(eval (eval (list 'cons t NIL))) ; undefined function T
(apply #'cons '(t NIL)) ; (T)
(eval NIL) ; NIL
(list 'eval NIL) ; (EVAL NIL) 
(eval (list 'eval NIL)) ; NIL