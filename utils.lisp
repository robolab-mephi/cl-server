; parse message string to string-and-number list
;"IAM NODE n1" -> ("IAM" "NODE" "n1")
;"WHOIS NODE n1" ->("WHOIS" "NODE" "n1")
;"PROGRAMSOF n1 <prog1 <<rf2d d1> <rf2d d1>>> <prog2 <<ss ss1>>" -> ("PROGRAMSOF" "n1" ("prog1" (("rf2d" "d1") ("rf2d" "d1"))) ("prog2" (("ss" "ss1"))))
; issues: check "t" --- convert to string?
(defun split-data (message)
  (labels ((for-tree (func tree)
	     (cond ((null tree) nil)
		   ((consp (car tree)) (cons (for-tree func (car tree))
					     (for-tree func (cdr tree))))
		   (t (cons (funcall func (car tree))
			    (for-tree func (cdr tree)))))))
    (for-tree #'(lambda (elem)
		  (cond ((symbolp elem) (string elem))
			(t elem)))
		    (read-from-string (concatenate 'string "(" (string-upcase message) ")")))))

; convert list to message
(defun serialize-data (data)
	(format nil "~A" data))

;	(if (consp data)
;      (string-trim " " (apply #'concatenate (cons 'string 
;						  (mapcar #'(lambda (el1) 
;							      (if (consp el1)
;								  (format nil "<~A> " (serialize-data el1))
;								  (format nil "~A " el1)))
;							  data))))
;      (format nil "~A" data)))
  

; byte array to string
(defun array-to-string (arr len) 
  (let ((sss (make-string len)))
    (dotimes (rr len) 
	(setf (char sss rr) (code-char (aref arr rr)))); !!! Быдлокод! Заменить на потоки
    sss))

; string to byte array
(defun string-to-array (str) 
  (let ((arr (make-array (length str) :element-type '(UNSIGNED-BYTE 8))))
    (dotimes (rr (length str)) 
	(setf (aref arr rr) (char-code (aref str rr)))); !!! Быдлокод! Заменить на потоки
    arr))


; compare first elements of list l-a and l-b
; !!! issue: add case-independent processing for strings
(defun compare-lists (l-a l-b)
  (cond ((null l-b) t)
	((or (null l-a) (not (equal (car l-a) (car l-b)))) nil)
	(t (compare-lists (cdr l-a) (cdr l-b)))))

