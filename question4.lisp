;  Team member 1: Harmanvir Singh - 40019114 (Team leader)
;  Team member 2: Sarabpreet Singh Rekhi - 40154067
;  Team member 3: Jad Al Kassis - 40168006
;  Assignment 2, Q4

(defun reverse-fun (lst)
		  (cond ((null lst) '())
		(t (append (reverse-fun (cdr lst)) (list (car lst))))))
		
		(defun flatten (lst)
		  (cond ((null lst) nil)
		        ((atom lst) (list lst))
		        (t (loop for a in lst appending (flatten a)))))
		        
		      
		(defun remove-dupli (lst)
		  (cond ((null lst) lst)
		        ((member (car lst) (cdr lst))(remove-dupli (cdr lst)))
		        (t (cons (car lst) (remove-dupli (cdr lst))))))
		        
		        
		(defun remove-char (lst)
		    (cond ((null lst) nil)
		          ((numberp (car lst)) (cons (car lst) (remove-char (cdr lst))))
		          (t (remove-char (cdr lst)))
		        )
		    )
		(defun flatten-nums-nodup (lst)
		(print (reverse-fun (remove-char (remove-dupli (reverse-fun (flatten lst))))))
		)   
		
		(flatten-nums-nodup '(1 2 (3 1) (a 2.5) (2 4.5) ((1 2))))