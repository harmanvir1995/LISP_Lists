;  Team member 1: Harmanvir Singh - 40019114 (Team leader)
;  Team member 2: Sarabpreet Singh Rekhi - 40154067
;  Team member 3: Jad Al Kassis - 40168006
;  Assignment 2, Q2

(defun sub-list2 (lst from &optional (to nil))
     ; If the "to" in null then  assign it the value of the length of the list    
     (if (null to)
            (setf to (length lst))
     )
     ; Size of the original list
     (setq SIZE (length lst))
     ; The recurssive helper function 
     (sub-listRecursive lst from to)
)

; The recursive helper function
(defun sub-listRecursive (lst from to)
    ; The length of the list passed to this function 
    ; Note it changes with every recursive call
    (setf N (length lst))
    
    (if (<= from 0)
        (setf from 1)
    )
    
    (if (> to SIZE)
        (setf to SIZE)
     )
    
    (cond 
        ; If the list is null then return nil
        ((null lst) nil)
        
        ; If from is larger than size than return nil
        ((> from SIZE) nil)
        
        ; if to is less than 0 return nil
        ((<= to 0) nil)
        ; Last condition it is called when none of the above 
        ; condition is called
        (t (if (and (<= N(+ 1 (- SIZE from))) (> N (- SIZE to)))
               (cons (car lst)(sub-listRecursive (cdr lst) from to))
               (sub-listRecursive (cdr lst) from to)
           )
         )
     )
)

(print 
    (sub-list2 '(1 4 10) 2 3)
 )

(print 
    (sub-list2 '(1 4 10) 2)
 )

(print 
    (sub-list2 '(1 7 12) 1 4)
 )

(print 
    (sub-list2 '(1 7 12) 0 1)
 )

(print 
    (sub-list2 '(1 6 12) 4 2)
 )

;(print (sub-list2 '(1 4 10)))