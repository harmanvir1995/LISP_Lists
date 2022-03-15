;  Team member 1: Harmanvir Singh - 40019114 (Team leader)
;  Team member 2: Sarabpreet Singh Rekhi - 40154067
;  Team member 3: Jad Al Kassis - 40168006
;  Assignment 2, Q3

(defun sub-list3 (lst from &optional (to nil))
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
    ; Size of the list at each iteration
    (setf N (length lst))
    ; if from is less than or equal to 0 then we set it to 1
    (if (<= from 0)(setf from 1))
    ; if to gets larger than the size of the original list 
    ; then we change it to the maximum size of the list
    (if (> to SIZE) (setf to SIZE))
    (cond
        ; If the list is null then simply return nil.
        ((null lst) nil)
        ; Only if length of the list in the bound then
        ; one of the following will happen
        ;if from is less than to, it will operate as it is
        ((and (<= N (+ 1 (- SIZE from))) (> N (- SIZE to)) (< from to)) (cons (car lst) (sub-listRecursive  (cdr lst) from to))) 
        ;if from is larger than to, it will switch their values
        ((and (<= N (+ 1 (- SIZE to))) (> N (- SIZE from)) (> from to)) (append (sub-listRecursive  (cdr lst) from to) (list(car lst)))) 
         
        ; If not other condition is meet, we don't add elements and continue recursion
         (t(sub-listRecursive (cdr lst) from to))
     )
)

; Examples

(print (sub-list3 '(1 4 10 11 12 13 ) 5 2))
(print (sub-list3 '(1 4 10 12 13) 3))
(print (sub-list3 '(1 7 12 12 52 86) 4 0))
