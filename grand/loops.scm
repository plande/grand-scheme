(define-module (grand loops)
  #:use-module (grand define-keywords)
  #:use-module (grand function)
  #:export (numbers)
  #:export-syntax (for collect))

;; Python-style for-loop and list comprehensions
;; "make you a python for great bad"

(define-syntax for
  (syntax-rules (in)
    ((for x in list actions . *)
     (for-each (lambda (x) actions . *) list))))

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
