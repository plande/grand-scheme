(define-module (grand numbers)
  #:use-module (grand syntax)
  #:use-module (grand examples)
  #:use-module (grand list)
  #:export (natural?
	    number/base 
	    digits/base))

(define (natural? x)
  (and (integer? x)
       (>= x 0)))

(define ((number/base base) (l ...))
  (fold-left (lambda (result factor power)
	       (+ result (* factor (expt base power))))
	     0
	     l
	     (iota (length l) (- (length l) 1) -1)))

(e.g.
 ((number/base 2) '(1 0 0)) ===> 4)

(e.g.
 ((number/base 10) '(1 0 0)) ===> 100)

(define ((digits/base  base) n)
  (map (lambda (n)
	 (modulo n base))
       (unfold-right-until zero? (lambda (n)
				   (quotient n base))
			   n)))

(e.g. 
 ((digits/base 2) 4) ===> '(1 0 0))

(e.g.
 ((digits/base 10) 140) ===> '(1 4 0))
