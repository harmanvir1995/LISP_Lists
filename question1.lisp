;  Team member 1: Harmanvir Singh - 40019114 (Team leader)
;  Team member 2: Sarabpreet Singh Rekhi - 40154067
;  Team member 3: Jad Al Kassis - 40168006
;  Assignment 2, Q1

; The function which will print the values in the list from a particular 
; index to the particular index.
(defun sub-list (lst from &optional (to nil))
    ; if option to is null then "to" is set to the 
    ; length of the List
    (if (null to) (setf to (length lst) ) )
    ; Constant variable which will not change its size 
    ; during the recursive calls
    (setq SIZE (length lst))
    ; Recursive Helper Function
    (sub-list1 lst from to)
 )

;Helper recursive function
(defun sub-list1 (lst from to)
        ; length that will be constantly updated
        (setf N (length lst) )
        ; condition statement
        (cond 
            ( (null lst) nil)
            ; condition to verify if the variable "from" is in the range (0, SIZE)
            ( ( or(<= from 0)(> from SIZE)) nil)
            ; condition to verify if the variable "to" is in the range (0, SIZE)
            ( ( or(<= to 0)(> to SIZE)) nil)
            ; This conditions activates if the others didn't
            ; With the "if" statement, we see if length of list is between difference original length and both form and to
            (t 
                 (if (and(<= N (+ 1 (- SIZE from)))(> N (- SIZE to)))
                    ; Recursion happens. If condition meet, we add the first element to the returned list
                    (cons (car lst) (sub-list1 (cdr lst) from to))
                    ; Else, we don't add elements and continue recursion
                    (sub-list1 (cdr lst) from to)
                  ) 
             )
        )
 )

; Printing the examples
(print (sub-list '(1 4 10) 2 3))
(print (sub-list '(1 4 10 ) 2))
(print (sub-list '(1 7 12) 1 4))
(print (sub-list '(1 7 12) 0 1))
(print (sub-list '(1 6 12) 4 2))
(print (sub-list '(1 6 12) ))