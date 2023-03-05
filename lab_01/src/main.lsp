(defvar *slist* '(1 2 3 4 5))

(defun list-nth (l idx)
  (case idx
    (2 (cadr l))
    (3 (caddr l))
    (4 (cadddr l))))

#(
  "LaTeX code block"
  (caadr '((blue cube)(red pyramid)))   ; -> red
  (cdar '((abc)(def)(ghi)))             ; -> Nil
  (cadr '((abc)(def)(ghi)))             ; -> (def)
  (caddr '((abc)(def)(ghi)))            ; -> (ghi)

  (list 'Fred 'and 'Wilma)               ; (fred and wilma)
  (list 'Fred '(and Wilma))             ; -> (fred (and wilma))
  (cons Nil Nil)                        ; -> (Nil)
  (cons T Nil)                          ; -> T
  (cons Nil T)                          ; -> (Nil . T)
  (list Nil)                            ; -> (Nil)
  (cons '(T) Nil)                       ; -> ((T))
  (list '(one two) '(free temp))        ; -> ((one two) (free temp))
  (cons 'Fred '(and Wilma))             ; -> (Fred and Wilma)
  (cons 'Fred '(Wilma))                 ; -> (Fred Wilma)
  (list Nil Nil)                        ; -> (Nil Nil)
  (list T Nil)                          ; -> (T Nil)
  (list Nil T)                          ; -> (Nil T)
  (cons T (list Nil))                   ; -> (T Nil)
  (list '(T) Nil)                       ; -> ((T) Nil)
  (cons '(one two) '(free temp))        ; -> ((one two) free temp)
)

(defun f1 (ar1 ar2 ar3 ar4)
    (list (list ar1 ar2) (list ar3 ar4)))

(defun f2 (ar1 ar2)
    (list (list ar1) (list ar2)))

(defun f3 (ar1)
  (list (list (list ar1))))

(defun f1-cons (ar1 ar2 ar3 ar4)
  (cons
   (cons ar1 (cons ar2 ()))
   (cons (cons ar3 (cons ar4 ())) ())))

(defun f2-cons (ar1 ar2)
  (cons
   (cons ar1 ())
   (cons (cons ar2 ()) ())))

(defun f3-cons (ar1)
  (cons
   (cons ar1 ())
   ()))
