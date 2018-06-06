(define-module (grand loops)
  #:use-module (grand define-keywords)
  #:use-module (grand function)
  #:use-module (grand examples)
  #:use-module (grand list)
  #:use-module (grand syntax)
  #:export (numbers)
  #:export-syntax (for collect))

;; Python-style for-loop and list comprehensions
;; "make you a python for great bad"



(define-syntax for
  (syntax-rules (in =>)
    ((for (key => value) in hash-map actions . *)
     (hash-for-each (lambda (key value) actions . *) hash-map))
    
    ((for x in list actions . *)
     (for-each (lambda (x) actions . *) list))))

(define/keywords (numbers #:from start #:= 0
			  #:to end
			  #:by step #:= 1)
  (let* ((step (* (if (is start > end) -1 1)
		  (if (positive? step) 1 -1)
		  step))
		  
	 (exceeding? (cond ((positive? step) <)
			   ((negative? step) >)
			   (else (lambda (x y) #true))))
	 (amount (floor (abs (/ (- end start) step)))))
  
    (define (build-down result #;from end)
      (if (is end exceeding? start)
	  result
	  (build-down `(,end . ,result)
		      (- end step))))
    
    (build-down '() (+ start (* amount step)))))

(define-syntax collect
  (syntax-rules (for in if)
    ((collect result)
     `(,result))
    
    ((collect result for variable in list . *)
     (append-map (lambda (variable)
		   (collect result . *))
		 list))
    
    ((collect result if condition . *)
     (if condition
	 (collect result . *)
	 '()))))

(e.g.
 (collect `(,x ,y ,z)
	  for z in (numbers #:from 1 #:to 20)
	  for y in (numbers #:from 1 #:to z)
	  for x in (numbers #:from 1  #:to y)
	  if (= (+ (* x x) (* y y))
		(* z z)))
 ===> ((3 4 5) (6 8 10) (5 12 13) (9 12 15) (8 15 17) (12 16 20)))
