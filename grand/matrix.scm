(define-module (grand matrix)
  #:use-module (grand syntax)
  #:use-module (grand list)
  #:use-module (grand examples)
  #:export (matrix? M* M+ det inv dim transpose diag zero matrix-ref))

(define (matrix? x)
  (and (list? x)
       (every (lambda (row) 
		(and (list? row) 
		     (every number? row))) 
	      x)
       (apply = (map length x))))

(e.g.
 (matrix? '((1 2 3)
	    (4 5 6)
	    (7 8 9))))

(define (dim x)
  (match x
    ((first . rest)
     `(,(length x) . ,(dim first)))
    (_ 
     '())))

(e.g. (dim '((1 2 3)
	     (4 5 6))) ===> (2 3))

(define (M+2 A B)
  #;(assert (and (matrix? A) 
	       (matrix? B) 
	       (equal? (dim A) (dim B))))
  (map (lambda (a b)
	 (map + a b))
       A B))

(define (M+ M . MM)
  #;(assert (and (matrix? M) 
	       (every matrix? MM)
	       (apply equal? (dim M) (map dim MM))))
  (fold-left M+2 M MM))

(e.g.
 (M+ '((1 2 3)
       (4 5 6)) '((1 2 3)
		  (4 5 6)) '((1 2 3)
			     (4 5 6))) ===> (( 3  6  9)
					     (12 15 18)))
(define (transpose M)
  #;(assert (matrix? M))
  (apply map list M))

(e.g.
 (transpose '((1 2 3)
	      (4 5 6))) ===> ((1 4)
			      (2 5)
			      (3 6)))

(define (M*2 A B)
  #;(assert (and (matrix? A)
	       (matrix? B)
	       (let (((A-rows A-cols) (dim A))
		     ((B-rows B-cols) (dim B)))
		 (= A-cols B-rows))))
  (let ((B^T (transpose B)))
    (map (lambda (rA)
	   (map (lambda (cB)
		  (sum (map * rA cB)))
		B^T))
	 A)))

(define (M* M . MM)
  (fold-left M*2 M MM))

(e.g.
 (M* '((1 2 3)
       (4 5 6)) '((7)
		  (8)
		  (9)) '((50 122))) ===> ((2500  6100)
					  (6100 14884)))

(define (crossout row column #;from M)
  #;(assert (and (matrix? M)
	       (integer? row)
	       (integer? column)
	       (let (((rows columns) (dim M)))
		 (and (<= 0 row (- rows 1)) 
		      (<= 0 column (- columns 1))))))
  (let* ((preceding-rows (_ . following-rows) (split-at M row))
	 (rows `(,@preceding-rows ,@following-rows)))
    (map (lambda (row)
	   (let ((preceding (_ . following) (split-at row column)))
	     `(,@preceding ,@following)))
	 rows)))

(e.g.
 (crossout 1 1 '(( 1  2  3  4 )
		 ( 5  6  7  8 )
		 ( 9 10 11 12 ))) ===> (( 1  3  4 )
					( 9 11 12 )))

(define (square-matrix? M)
  (and (matrix? M)
       (let (((rows columns) (dim M)))
	 (= rows columns))))

(define (matrix-ref M . indices)
  (fold-left list-ref M indices))

(e.g. (matrix-ref '((1 2)
		    (3 4)) 0 1) ===> 2)


(define (det M)
  #;(assert (square-matrix? M))
  (match M
    (((item))
     item)
    ((first-row . other-rows)
     (let ((columns (length first-row)))
       (sum (map (lambda (n)
		 (* (matrix-ref M 0 n)
		    (det (crossout 0 n M))
		    (expt -1 n)))
	       (iota columns)))))))

(e.g. ;; cf. http://www.mathsisfun.com/algebra/matrix-determinant.html
 (det '((3 8)
	(4 6))) ===> -14)

(e.g.
 (det '(( 6  1  1 )
	( 4 -2  5 )
	( 2  8  7 ))) ===> -306)

(define (scale-matrix M #;by factor)
  #;(assert (and (matrix? M)
	       (number? factor)))
  (map (lambda (row)
	 (map (lambda (item)
		(* item factor))
	      row))
       M))

(e.g.
 (scale-matrix '((1 0 0)
		 (0 1 0)
		 (0 0 1)) #;by 2) ===> ((2 0 0)
					(0 2 0)
					(0 0 2)))


(define (diag M)
  #;(assert (matrix? M))
  (map list-ref M (iota (length M))))

(e.g. (diag '((1 2 3)
	      (4 5 6)
	      (7 8 9))) ===> (1 5 9))

(define (zero . dimensions)
  (match dimensions
    (()
     0)
    ((n . rest)
     (let ((zeros (apply zero rest)))
       (unfold-right-upto n identity zeros)))))

(e.g.
 (zero 2 3)
 ===> ((0 0 0)
       (0 0 0)))
