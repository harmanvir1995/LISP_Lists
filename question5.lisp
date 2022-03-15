;  Team member 1: Harmanvir Singh - 40019114 (Team leader)
;  Team member 2: Sarabpreet Singh Rekhi - 40154067
;  Team member 3: Jad Al Kassis - 40168006
;  Assignment 2, Q5

;part (a)

(defun get-element (lst pos)
  (cond ((null lst) NIL)
	((or (> pos (lst-length list)) (< pos 1)) NIL)
	((= 1 pos) (car lst))
	(t (get-element (cdr lst) (- pos 1)))
  )
  )

(defun tribonacci-seq(num)
  (setq tribonacci-lst '()) 
  (dotimes (n num)
    ;(print tribonacci-lst)
    (cond ((< n 2) (setq tribonacci-lst (cons 0 tribonacci-lst)))
	  ((= n 2) (setq tribonacci-lst (cons 1 tribonacci-lst)))
	  (t (setq s1 (car tribonacci-lst))
	     (setq s2 (car (cdr tribonacci-lst)))
	     (setq s3 (car (cdr (cdr tribonacci-lst))))
	     (setq new-element (+ (+ s1 s2) s3))
	     (setq tribonacci-lst (cons new-element tribonacci-lst))
	     )
	  )
    )
  (print (reverse tribonacci-lst))
  )
;Examples:
(tribonacci-seq 7)
(tribonacci-seq 0)
(tribonacci-seq 1)

----------------------------------------------------------------------------------------
;part(b)
;Defining a helper function
;returns list of n tribonacci elements after x,y and z
;If n is 0, it returns an empty list
;Otherwise cons next element (x+y+z) to recursive call with n reduced
;and arguments changed to y,z and (+ x y z)
(defun helper (n x y z)
    (if (= n 0) '()
        (cons (+ x y z) (helper (- n 1) y z (+ x y z)))
    )
)
;required function
;If n is less than 1, return empty list
;If n is 1, return list with one element only
;If n is 2, return list with two 0s
;Otherwise append ' (0 0 1) to next n-3 elements of the sequence using helper
(defun tribonacci (n)
(cond ((< n 1 ) '())
      ((= n 1)'(0))
      ((= n 2)'(0 0))
      (t (append '(0 0 1) (helper (- n 3) 0 0 1)))
    )
)

;Examples:
(print (tribonacci 7))
(print (tribonacci 0))
(print (tribonacci 1))
