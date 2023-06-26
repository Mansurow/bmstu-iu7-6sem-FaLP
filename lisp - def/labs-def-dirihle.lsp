(defvar beta 0.5)
(defvar f0 300)
(defvar xmax 10.0)
(defvar zmax 10.0)
(defvar n 10)
(defvar m 10)
(defvar hx (/ xmax n))
(defvar hz (/ zmax n))
(defvar x0 5)
(defvar z0 5)
(defvar F0 0)
(defvar T0 300) 
(defvar tau 1)
(defvar eps 0.00001)

; верно
(defun f (x z)
    (* f0 (exp (* (- 0 beta)
                  (- x x0)
                  (- x x0)
                  (- z z0)
                  (- z z0) 
               )
          )
    )
)

(defun create-list-h (size step i)
    (if (< size 0)
        nil
        (cons (* i step) (create-list-h (1- size) step (1+ i)))
    )
)

(defun xRight ()
    T0
)

(defun xLeft ()
    T0
)

(defun zRight ()
    T0
)

(defun zLeft ()
    T0
)

(defun create-list-zLimit (size)
    (if (zerop size)
        nil
        (cons T0 (create-list-zLimit (1- size)))
    )
)

(defun create-list-xLimit (size i)
    (if (>= i size)
        nil
        (if (zerop i)
            (cons T0 (create-list-xLimit size (1+ i)))
            (if (= i (1- n))
                (cons T0 (create-list-xLimit size (1+ i)))
                (cons 0 (create-list-xLimit size (1+ i)))
            )
        )
    )
)

(defun create-uList (n m i)
    (if (>= i n)
        nil
        (if (or (zerop i) (= i (1- n)))
            (cons (create-list-zLimit m) (create-uList n m (1+ i)))
            (cons (create-list-xLimit m 0) (create-uList n m (1+ i)))
        )
    )
)

(print (create-list-h n hx 0))
(print (create-list-h m hz 0))
(print (create-uList n m 0))


(defun task-dirihle ()
    (let (dMax 100)
        (if (> dMax eps)
            
        )
    )
)


(defun print-list (lst)
  (when lst
    (format t "~a " (car lst))
    (print-list (cdr lst))
   )
)

(defun print-result (x z y)
    (print-list x)
    (print-list z)
    (print-list y)      
)

(defun modelConstant()
    (let (
            (x (create-list-h n hx 0))
            (z (create-list-h m hz 0))
        )

        (print-result x z nil)
    )
)

;(modelConstant)