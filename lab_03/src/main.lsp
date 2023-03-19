; 1. Написать функцию, которая принимает целое число и возвращает первое 
;    четное число, не меньшее аргумента.
(defun first_even (num)
    (if (numberp num)
        (if (evenp num)
            num
            (+ num 1)) 
    nil)
)

(defun first_even_2 (num)
    (if (numberp num)
        (if (= (mod num 2) 0)
            num
            (+ num 1)) 
    nil)
)

; (print (first_even 'a))
; (print (first_even 13))
; (print (first_even 14))

; (print (first_even_2 'a))
; (print (first_even_2 13))
; (print (first_even_2 14))

; 2. Написать функцию, которая принимает число и возвращает число 
;    того же знака, но с модулем на 1 больше модуля аргумента.

(defun inc_mod (num)
    (if (> num 0)
        (+ num 1)
        (- num 1)
    )
)

; (print (inc_mod 14))
; (print (inc_mod -14))

; 3. Написать функцию, которая принимает два числа и возвращает 
;    список из этих чисел, расположенный по возрастанию.

(defun my_sort (a b)
    (if (< a b)
        (list a b)
        (list b a)
    )
)

; (print (my_sort 13 14))
; (print (my_sort 15 14))

; 4. Написать функцию, которая принимает три числа и возвращает Т только
;    тогда, когда первое число расположено между вторым и третьим.

(defun number_among_other (v1 v2 v3)
    (or (and (< v2 v1) (< v1 v3))
        (and (< v3 v1) (< v1 v2))
    )
)

; (print (number_among_other 14 -12 15)) ; T
; (print (number_among_other 16 12 15))  ; Nil
; (print (number_among_other 14 18 12))  ; T
; (print (number_among_other 16 -19 12)) ; Nil

; 5. Каков результат вычисления следующих выражений?

; (print (and 'fee 'fie 'foe)) ; foe
; (print (or nil 'fie 'foe))   ; fie
; (print (and (equal 'abc 'abc) 'yes)) ; yes
; (print (or 'fee 'fie 'foe)) ; fee
; (print (and nil 'fie 'foe)) ; nil
; (print (or (equal 'abc 'abc) 'yes)) ; T

; 6. Написать предикат, который принимает два числа-аргумента и возвращает
;    Т, если первое число не меньше второго

(defun equal-or-bigger (v1 v2)
    (>= v1 v2)
)

(defun equal-or-bigger_2 (v1 v2)
    (or (> v1 v2) (= v1 v2))
)

; (print (equal-or-bigger 1 2)) ; Nil
; (print (equal-or-bigger 2 2)) ; T
; (print (equal-or-bigger 3 2)) ; T

; 7. Какой из следующих двух вариантов предиката ошибочен и почему?

(defun pred1 (x) 
    (and (numberp x) (plusp x))
)
    
(defun pred2 (x) 
    (and (plusp x) (numberp x))
)

; (print (pred1 12)) ; T
; (print (pred2 12)) ; T
; (print (pred1 11)) ; T
; (print (pred2 11)) ; T
; (print (pred1 'a)) ; Nil
; (print (pred2 'a)) ; Error

; 8. Решить задачу 4, используя для ее решения конструкции:
;    только IF, только COND, только AND/OR.

; AND/OR
(defun number_among_other_and_or (v1 v2 v3)
    (or (and (< v2 v1) (< v1 v3))
        (and (< v3 v1) (< v1 v2))
    )
)
; IF
(defun number_among_other_if (v1 v2 v3)
    (if (< v2 v1)
        (if (< v1 v3) T nil)
        (if (< v3 v1) T nil)
    )
)
; COND
(defun number_among_other_cond (v1 v2 v3)
    (cond ((< v2 v1) (cond 
                         ((< v1 v3) T)
                         (T nil)))           
          ((< v1 v2) (cond 
                         ((< v3 v1) T)
                         (T nil)))             
          (T nil))
)

; (print (number_among_other_if 14 -12 15)) ; T
; (print (number_among_other_cond 14 -12 15)) 

; (print (number_among_other_if 16 12 15))  ; Nil
; (print (number_among_other_cond 16 12 15)) 

; (print (number_among_other_if 14 18 12))  ; T
; (print (number_among_other_cond 14 18 12))

; (print (number_among_other_if 16 -19 12)) ; Nil
; (print (number_among_other_cond 16 -19 12)) 

; (print (number_among_other_if 17 17 17)) ; Nil
; (print (number_among_other_cond 17 17 17))  

; 9. Переписать функцию how-alike, приведенную в лекции и использующую COND, используя
;    только конструкции IF, AND/OR.

(defun how_alike (x y)
    (cond ((or (= x y) (equal x y)) 'the_same)
          ((and (oddp x) (oddp y)) 'both_odd)
          ((and (evenp x) (evenp y)) 'both_even)
          (t 'difference) 
   ) 
)

; AND/OR
(defun how_alike_and_or (x y)
    (or (and (or (= x y) (equal x y)) 'the_same)
        (and (and (oddp x) (oddp y)) 'both_odd)
        (and (and (evenp x) (evenp y)) 'both_even)
        'difference
    )
)
; IF
(defun how_alike_if (x y)
    (if (= x y)
        'the_same
        (if (equal x y) 'the_same
            (if (oddp x)
                (if (oddp y) 'both_odd 
                    (if (evenp x)
                        (if (evenp y) 'both_even 'difference)
                        'difference
                    )
                )
                (if (evenp x)
                    (if (evenp y) 'both_even 'difference)
                    'difference
                )
            )
        )
    )
)

(print (how_alike 1 1)) ; the_same
(print (how_alike_and_or 1 1))
(print (how_alike_if 1 1))

(print (how_alike 2 4)) ; both_even
(print (how_alike_and_or 2 4)) 
(print (how_alike_if 2 4))

(print (how_alike 3 6)) ; difference
(print (how_alike_and_or 3 6)) 
(print (how_alike_if 3 6)) 

(print (how_alike 3 7)) ; both_odd
(print (how_alike_and_or 3 7)) 
(print (how_alike_if 3 7)) 