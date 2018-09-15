(define-module (grand set)
  #:use-module (grand syntax)
  #:use-module (srfi srfi-1)
  #:use-module (grand examples)
  #:export (union difference intersection equivalence-classes in? same-sets? same-set?
		  subset? member? set powerset))

(define (union . args)
  (apply lset-union equal? args))

(define (difference . args)
  (apply lset-difference equal? args))

(define (intersection . args)
  (apply lset-intersection equal? args))

(define (equivalence-classes equivalent? set)
  (let next-item ((set set)(result '()))
    (match set
      (()
       (reverse (map reverse result)))
      ((item . set)
       (match result
	 (()
	  (next-item set `((,item) . ,result)))
	 ((this . next)
	  (let next-class ((past '()) (present this) (future next))
	    (match present
	      ((paradigm . _)
	       (if (equivalent? item paradigm)
		   (next-item set `((,item . ,present)
				    . (,@past ,@future)))
		   (match future
		     (()
		      (next-item set `((,item) ,@result)))
		     ((this . next)
		      (next-class `(,present . ,past) this next)))
		   )))
	    )))))))

(e.g.
 (equivalence-classes (lambda(x y)(= (modulo x 3) (modulo y 3))) (iota 9))
 ===> ((0 3 6) (1 4 7) (2 5 8)))


(define ((in? set) element)
  (match set
    ((first . rest)
     (or (equal? element first)
	 ((in? rest) element)))
    (_
     #f)))

(e.g. ((in? '(a b c)) 'a))

(e.g. (not ((in? '(a b c)) 'd)))

(define (same-sets? . sets)
  (apply lset= equal? sets))

(define same-set? same-sets?)
		   
(define (subset? . sets)
  (apply lset<= equal? sets))

(define member? member)

(define set delete-duplicates)

(define (powerset s)
  (match s
    (()
     '(()))
    ((h . t)
     (let ((r (powerset t)))
       `(,@(map (lambda (l)
		  `(,h . ,l))
		r)
	 ,@r)))))

(e.g.
 (powerset '(1 2 3))
 ===> ((1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) ()))
