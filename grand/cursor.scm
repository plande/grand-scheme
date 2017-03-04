(define-module (grand cursor)
  #:use-module (grand syntax)
  #:use-module (grand examples)
  #:use-module (grand list)
  #:use-module (grand function)
  #:use-module (grand improper)
  #:use-module (grand numbers)
  #:export (cursor?
	    cursor-focus
	    
	    cursor-next
	    cursor-previous
	    cursor-points-to-location?
	    cursor-points-to-expression?
	    cursor-embracing?
	    cursor-parent+location
	    cursor-depth

	    splice-subexpression
	    replace-subexpression
	    remove-subexpression
	    )
  )

(define (cursor? x)
  (or (natural? x)
      (null? x)
      (and-let* (((h . t) x)
		 ((natural? h))
		 ((cursor? t))))))

(e.g.
 (and (cursor? 0)
      (cursor? '())
      (cursor? '(0 1 0))
      (cursor? '(0 1 0 . 0))))

(define (cursor-parent+location cursor)
  (match cursor
    ((prefix . rest)
     (let ((expression location (cursor-parent+location rest)))
       (values `(,prefix . ,expression) location)))
    (_
     (values '() cursor))
    ))

(e.g.
 (cursor-parent+location '(1 2 . 3)) ===> (1 2) 3)

(define (cursor-focus expression #;on cursor)
  "Select a subexpression pointed to by a cursor."
  (match cursor
    (()
     expression)
    ((h . t)
     (cursor-focus (list-ref expression h) #;on t))
    (_
     (drop expression cursor))
    ))

(e.g.
 (cursor-focus '(+ (* 2 3) (/ 3 5)) '(1 0)) ===> *)

(e.g.
 (cursor-focus '(+ (* 2 3) (/ 3 5)) '(1 . 1)) ===> (2 3))

(define (cursor-next #;to cursor #;in expression)
  (match `(,expression ,cursor)
    (((_ . _) ())
     0)
    ((() ())
     0)
    (((head . tail) (n . next))
     (let* ((subexpression (list-ref expression n))
	    (subcursor (cursor-next #;to next #;in subexpression)))
       (if (null? subcursor)
	   (+ n 1)
	   `(,n . ,subcursor))))
    (((head . tail) n)
     (let ((subexpression (drop expression n)))
       (if (pair? subexpression)
	   `(,n)
	   '())))
    (_
     '())
    ))

(e.g.
 (let ((exp '(+ (* 1 2) (/ 3 4))))
   (unfold-left-until null? (lambda (c)
			      (cursor-next c exp))
		      #;starting-from 0))
 ===> (0 (0) 1 (1) (1 . 0) (1 0) (1 . 1) (1 1) (1 . 2) (1 2) (1 . 3)
	 2 (2) (2 . 0) (2 0) (2 . 1) (2 1) (2 . 2) (2 2) (2 . 3) 3))

(define (cursor-previous #;to cursor #;in expression)

  (define (some-previous-cursor cursor)
    (match cursor
      (()
       (length. expression))
      ((0)
       0)
      ((super ... n)
       `(,@super . ,n))
      (_
       (let* ((parent location (cursor-parent+location cursor)))
	 (if (= location 0)
	     parent
	     `(,@parent ,(- location 1)))))))

  (let ((cursors (unfold-right-until
		  (lambda (c) (or (equal? c cursor) (null? c)))
		  #;using (lambda (c) (cursor-next #;to c #;in expression))
			  #;starting-from (some-previous-cursor cursor))))
    (match cursors
      ((previous . _)
       previous)
      (_
       '())
      )))

(e.g.
 (let ((exp '(+ (* 1 2) (/ 3 4))))
   (unfold-right-until null? (lambda (c)
			       (cursor-previous c exp))
		       #;starting-from 3))
 ===> (0 (0) 1 (1) (1 . 0) (1 0) (1 . 1) (1 1) (1 . 2) (1 2) (1 . 3)
	 2 (2) (2 . 0) (2 0) (2 . 1) (2 1) (2 . 2) (2 2) (2 . 3) 3))

(define (cursor-points-to-location? cursor)
  ;;(assert (cursor? cursor))
  (or (natural? cursor)
      (and-let* (((n . subcursor) cursor))
	(cursor-points-to-location? subcursor))))

(define (cursor-points-to-expression? cursor)
  ;;(assert (cursor? cursor))
  (list? cursor))

(define (splice-subexpression x #;to expression #;at cursor)
  ;;(assert (points-to-location? cursor))
  (match cursor
    ((n . subcursor)
     (let ((prefix (subexpression . suffix) (split-at expression n)))
       `(,@prefix
	 ,(splice-subexpression x #;to subexpression #;at subcursor)
	 ,@suffix)))
    (_
     (let ((prefix suffix (split-at expression cursor)))
       (if (null? suffix)
	   `(,@prefix ,@x) ;; handle dotted pairs
	   `(,@prefix ,@x ,@suffix))))
    ))

(e.g.
 (splice-subexpression '(c) #;to '(* (+ a b) (/ d e)) #;at '(1 . 3))
 ===> (* (+ a b c) (/ d e)))

(define (replace-subexpression #;of expression #;at cursor #;with x)
  (match cursor
    ((n . subcursor)
     (let ((prefix (subexpression . suffix) (split-at expression n)))
       `(,@prefix
	 ,(replace-subexpression #;of subexpression #;at subcursor
				      #;with x)
	 ,@suffix)))
    (()
     x)
    (_
     `(,@(take expression cursor) ,@x))
    ))

(e.g.
 (replace-subexpression #;of '(* (+ a 1) (/ c d)) #;at '(1 2)
			     #;with 'b)
 ===> (* (+ a b) (/ c d)))

(define (remove-subexpression #;of expression #;at/after cursor)
  (cond ((natural? cursor)
	 (take expression cursor))
	((null? cursor)
	 '())
	(else
	 (let* (((n . subcursor) cursor)
		(prefix (subexpression . suffix) (split-at expression n)))
	   `(,@prefix
	     ,@(if (null? subcursor)
		   '()
		   `(,(remove-subexpression #;of subexpression
						 #;at subcursor)))
	     ,@suffix)))
	))

(e.g.
 (remove-subexpression #;of '(* (+ a c b) (/ c d)) #;at '(1 2))
 ===> (* (+ a b) (/ c d)))

(e.g.
 (remove-subexpression #;of '(* (+ a b c) (/ c d)) #;after '(1 . 1))
 ===> (* (+) (/ c d)))

(define (cursor-embracing? outer inner)
  "Is outer cursor embracing inner?"
  (or (null? outer)
      (and-let* (((a . a*) outer)
		 ((b . b*) inner)
		 ((= a b))
		 ((cursor-embracing? a* b*))))))

(e.g.
 (cursor-embracing? '(1 0) '(1 0 0 . 1)))

(define (cursor-depth cursor)
  (match cursor
    ((n . subcursor)
     (+ 1 (cursor-depth subcursor)))
    (()
     0)
    (_
     1)
    ))

(e.g.
 (cursor-depth '(1 2 . 3)) ===> 3)
