; (1 (2 () 3) 4 (5 6 . 7) 8 ((9 A)) B) 
(
print (cons 1 
            (cons (cons 2 
                        (cons nil 
                              (cons 3 
                                    nil))) 
                  (cons 4 
                        (cons (cons 5 
                                    (cons 6 7)) 
                              (cons 8 
                                    (cons (cons (cons 9 
                                                      (cons 'a 
                                                            nil)) 
                                                nil) 
                                          (cons 'b 
                                                nil)))))))
)