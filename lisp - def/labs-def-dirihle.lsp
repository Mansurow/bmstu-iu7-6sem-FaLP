(defvar beta 0.5)
(defvar f0 30)
(defvar xmax 10.0)
(defvar zmax 10.0)
(defvar n 10)
(defvar m 10)
(defvar hx (/ xmax n))
(defvar hz (/ zmax n))
(defvar x0 5)
(defvar z0 5)
;(defvar F0 0)
(defvar T0 300) 
(defvar tau 1)
(defvar eps 0.001)

; верно
; (defun f (x z)
;     0
; ) 

; крест
(defun f (x z)
    (* f0 (exp (* (- 0 beta)
                  (+ (* (- x x0)
                  (- x x0))
                  ( * (- z z0)
                  (- z z0))) 
               )
          )
    )
)

; купол
#| (defun f (x z)
    (* f0 (exp (* (- 0 beta)
                  (- x x0)
                  (- x x0)
                  (- z z0)
                  (- z z0)
               )
          )
    )
) |#

(defun create-list-h (size step i)
    (if (<= size 0)
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

;(print (create-list-h n hx 0))
;(print (create-list-h m hz 0))
;(print (create-uList n m 0))

(defun getListValue (lst i j)
    (nth j (nth i lst))
)

(defun update-list (lst i j val)
  (if (= i 0)
      (cons (update-row (car lst) j val) (cdr lst))
      (cons (car lst) (update-list (cdr lst) (- i 1) j val))))

(defun update-row (row j val)
  (if (= j 0)
      (cons val (cdr row))
      (cons (car row) (update-row (cdr row) (- j 1) val))))

(defun process-u (u x z i j dmax)
  (if (< i (- n 1))
      (if (< j (- m 1))
          (let* ((tmp (* (+ (+ 
                                (getListValue u i (1+ j))
                                (getListValue u i (1- j))
                                (getListValue u (1+ i) j)
                                (getListValue u (1- i) j)
                            )
                            (* hx hz (f (nth i x) (nth j z)))
                        )
                        0.25))
                 (dm (abs (- tmp (getListValue u i j))))
                 (new-u (update-list u i j tmp))
                 (new-dmax (if (< dmax dm) dm dmax)))
            (process-u new-u x z i (+ j 1) new-dmax)
          )
          (process-u u x z (+ i 1) 1 dmax)
       )
      (cons u dmax)
    )
)

(defun calculate-dmax (u x z dmax k)
  (let* ((result (process-u u x z 1 1 dmax))
         (new-u (car result))
         (new-dmax (cdr result))
         (new-k (+ k 1)))
    (if (and (> new-k 1000) (> new-dmax eps))
        new-u
        (calculate-dmax new-u x z new-dmax new-k))))


(defun print-list (lst)
  (when lst
    (format t "~a " (car lst))
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

(defun print-result (x z y)
    (print-list x)
    (terpri)
    (print-list z)
    (terpri)
    (terpri)
    (print-double-list y)      
)

(defun modelConstant()
    (let ((u  (create-uList n m 0))
       (x (create-list-h n hx 0))
       (z (create-list-h m hz 0))
     )
        (print-result x z (calculate-dmax u x z 100 0))
    )
)

(modelConstant)

; (let ((u '((300 300 300 300 300 300 300 300 300 300) 
;            (300 0 0 0 0 0 0 0 0 300)
;            (300 0 0 0 0 0 0 0 0 300) 
;            (300 0 0 0 0 0 0 0 0 300) 
;            (300 0 0 0 0 0 0 0 0 300)
;            (300 0 0 0 0 0 0 0 0 300) 
;            (300 0 0 0 0 0 0 0 0 300) 
;            (300 0 0 0 0 0 0 0 0 300)
;            (300 0 0 0 0 0 0 0 0 300) 
;            (300 300 300 300 300 300 300 300 300 300)
;            )
;        )
;        (x (create-list-h n hx 0))
;        (z (create-list-h m hz 0))
;      )
;     (print (calculate-dmax u x z 100 0))
; )

; (let ( (u '((1 2 3)
;             (4 25 6)
;             (7 8 9)
;            )
;         )
;      )
;     (setf (nth 0 u) '(10 10 10))
;     (print u)
; )