(define-module (grand list)
  #:use-module (grand syntax)
  #:use-module (srfi srfi-1)
  #:use-module (grand examples)
  #:use-module (grand publishing)
  #:use-module (grand function)
  #:export (
	    only
	    without-first
	    unique?
	    argmin
	    argmax min+max argmin+argmax
	    omit
	    skip
	    alter
	    insert
	    splice
	    generate-list
	    fold-left
	    unfold-left
	    unfold-left-upto
	    unfold-left-until
	    unfold-right
	    unfold-right-upto
	    unfold-right-until
	    sum
	    product
	    scan
	    prefix-sum
	    prefix?
	    proper-prefix?
	    suffix?
	    proper-suffix?
	    extend-left
	    extend-right
	    indexed
	    unzip
	    map/values
	    map-n
	    take-upto
	    take-right-while
	    drop-upto
	    drop-right-while
	    find
	    rest
	    list<-values
	    weave
	    intersperse
	    chunks
	    none
	    proper-list+dotted-tail
	    flatten
	    )
  #:re-export (every
	       any
	       filter-map
	       append-map
	       concatenate
	       take
	       take-right
	       take-while
	       drop
	       drop-right
	       drop-while
	       partition
	       split-at
	       span
	       break
	       iota
	       count
	       first last
	       zip
	       first second third fourth fifth
	       fold-right
	       delete-duplicates
	       find-tail
	       )
  )

(define only filter)

(e.g.
 (only even? '(1 2 3 4)) ===> (2 4))

(define (without-first satisfying? list)
  (match list
    ('()
     list)
    (`(,head . ,tail)
     (if (satisfying? head)
	 tail
	 `(,head . ,(without-first satisfying? tail))))))

(e.g.
 (without-first even? '(1 2 3 4 5)) ===> (1 3 4 5))

(define (none pred . ls)
  (not (apply any pred ls)))

(define (unique? l)
  (or (null? l)
      (and-let* (((h . t) l)
		 (not (member h t)))
	(unique? t))))

(define-syntax list<-values
  (syntax-rules ()
    ((_ call)
     (call-with-values (lambda () call) list))))

(define rest cdr)

(publish
 (define (argmin property element . elements)
   (apply argopt < property element elements))
 (define (argmax property element . elements)
   (apply argopt > property element elements))
 where
 (define (argopt < property element . elements)
   (let next-trial ((champion element)
		    (mastery (property element))
                    (champion-index 0)
                    (rival-index 1)
		    (opponents elements))
     (if (null? opponents)
	 (values champion mastery champion-index)
	 (let* ((rival (first opponents))
		(challenge (property rival)))
	   (if (is challenge < mastery)
	       (next-trial rival
                           challenge
                           rival-index
                           (+ rival-index 1)
                           (rest opponents))
	       (next-trial champion
                           mastery
                           champion-index
                           (+ rival-index 1)
                           (rest opponents))))))))

(e.g.
 (argmin length '(1 2 3 4) '(5 6 7) '(8 9) '(10))
 ===> (10) 1 3)

(define (min+max first . args)
  #;(assert (and (number? first)
	       (every number? args)))
  (let loop ((min first)
	     (max first)
	     (remaining args))
    (match remaining
      (()
       (values min max))
      ((current . remaining)
       (cond ((< current min)
	      (loop current max remaining))
	     ((> current max)
	      (loop min current remaining))
	     (else
	      (loop min max remaining)))))))

(e.g.
 (min+max 5 4 6 3 7 2 8 1)
 ===> 1 8)

(define (argmin+argmax property element . elements)
  (let ((quality (property element)))
    (let next-trial ((winner element)
		     (looser element)
		     (mastery quality)
		     (failure quality)
		     (opponents elements))
      (if (null? opponents)
	  (values looser winner)
	  (let* ((rival (first opponents))
		 (quality (property rival)))
	    (cond ((is quality < failure)
		   (next-trial winner rival mastery quality 
			       (rest opponents)))
		  ((is quality > mastery)
		   (next-trial rival looser quality failure 
			       (rest opponents)))
		  (else
		   (next-trial winner looser mastery failure 
			       (rest opponents)))))))))

(e.g.
 (argmin+argmax length '(1 2) '(3) '(4 5 6))
 ===> (3) (4 5 6))

(define (skip #;element-number n #;in list)
  (let (((head . tail) list))
    (if (= n 0)
	tail
	`(,head . ,(skip #;element-number (- n 1) #;in tail)))))

(e.g. (skip #;element-number 1 #;in '(a b c)) ===> (a c))

(define (alter #;element-number n #;in list #;with replacement)
  #;(assert (and (integer? n) (>= n 0)))
  (let (((head . tail) list))
    (if (= n 0)
	`(,replacement . ,tail)
	`(,head . ,(alter #;element-number (- n 1) 
					   #;in tail
						#;with replacement)))))

(e.g.
 (alter #;element-number 1 #;in '(ząb dupa zębowa) #;with 'zupa)
 ===> (ząb zupa zębowa))

(define (omit k #;elements-at i #;in list)
  (let* ((prefix list (split-at list i))
	 (suffix (drop list k)))
    `(,@prefix ,@suffix)))

(e.g.
 (omit 3 #;elements-at 2 #;in '(0 1 2 3 4 5 6))
 ===> (0 1 5 6))

(define (insert element #;into list #;at position)
  (let ((prefix suffix (split-at list position)))
    `(,@prefix ,element ,@suffix)))

(e.g.
 (insert 'x #;into '(a b c) #;at 2)
 ===> (a b x c))

(define (splice sublist #;into list #;at position)
  (let ((prefix suffix (split-at list position)))
    `(,@prefix ,@sublist ,@suffix)))

(e.g.
 (splice '(x y) #;into '(u v w z) #;at 3)
 ===> (u v w x y z))

(define (generate-list n generator)
  #;(assert (and (integer? size) (>= size 0)))
  (let loop ((n n)
	     (l '()))
    (if (= n 0)
	l
	(loop (- n 1) `(,(generator) . ,l)))))


(define (fold-left op e . ls)
  (match ls
    (((heads . tails) ...)
     (apply fold-left op (apply op e heads) tails))
    (_
     e)))

;; the srfi-1 fold-right variant is good, so we'll just re-export it

(define (unfold-left f #;from . seed)
  (call-with-values (lambda () (apply f seed))
    (lambda result
      (match result
	(`()
	 '())
	(_
	 `(,@seed ,@(apply unfold-left f result)))))))

(e.g.
 (unfold-left (lambda (x)
		(if (is x > 10)
		    (#;no values)
		    (+ x 1)))
	      #;from 0)
 ===> (0 1 2 3 4 5 6 7 8 9 10))

(define (unfold-left-until stop? #;using f #;starting-with seed)
  (if (stop? seed)
      '()
      `(,seed . ,(unfold-left-until stop? #;using f
				    #;starting-with (f seed)))))

(define (unfold-left-upto n #;using f #;starting-with seed)
  (if (<= n 0)
      '()
      `(,seed . ,(unfold-left-upto (- n 1) #;using f
				   #;starting-with (f seed)))))

(define (unfold-right f #;from . seed)
  (define (unfold f #;into results #;from . seed)
    (call-with-values (lambda () (apply f seed))
      (lambda result
	(match result
	  (`()
	   results)
	  (_
	   (apply unfold f #;into `(,@seed ,@results) #;from result))))))
  (apply unfold f #;into '() #;from seed))

(e.g.
 (unfold-right (lambda (x)
		 (if (is x > 10)
		     (#;no values)
		     (+ x 1)))
	       #;from 0)
 ===> (10 9 8 7 6 5 4 3 2 1 0))

(define (unfold-right-until stop? #;using f #;starting-with seed)
  (define (unfold seed result)
    (if (stop? seed)
	result
	(unfold (f seed) `(,seed . ,result))))
  (unfold seed '()))

(define (unfold-right-upto n #;using f #;starting-with seed)
  (define (unfold n seed result)
    (if (is n <= 0)
	result
	(unfold (- n 1) (f seed) `(,seed . ,result))))
  (unfold n seed '()))


(define (sum elements)
  (fold-left + 0 elements))

(define (product elements)
  (fold-left * 1 elements))

(define (scan op e l)
  (match l
    (()
     '())
    ((h . t)
     (let ((e* (op e h)))
       `(,e* . ,(scan op e* t))))))

(e.g.
 (scan * 1 '(1 2 3 4 5 6)) ===> (1 2 6 24 120 720))

(define (prefix-sum l)
  (scan + 0 l))


(define* (extend-right l #;to size #;with #:optional (fill #f))
  (let ((extension-size (- size (length l))))
    (if (< extension-size 0)
	(error "list length exceeds the desired length")
	`(,@l ,@(make-list extension-size fill)))))

(e.g. (extend-right '(1 2 3) 5 0) ===> (1 2 3 0 0))

(define* (extend-left l #;to size #;with #:optional (fill #f))
  (let ((extension-size (- size (length l))))
    (if (< extension-size 0)
	(error "list length exceeds the desired length")
	`(,@(make-list extension-size fill) ,@l))))

(e.g. (extend-left '(1 2 3) 5 0) ===> (0 0 1 2 3))

(define (indexed list)
  (zip (iota (length list)) list))

(e.g. (indexed '(a b c)) ===> ((0 a) (1 b) (2 c)))

(define (unzip-n n l)
  (apply values (take (apply map list l) n)))

(define (unzip list)
  (match list
    (`(,first . ,rest)
     (unzip-n (length first) list))
    ('()
     '())))

(e.g.
 (unzip '((1 a) (2 b) (3 c))) ===> (1 2 3) (a b c))

(define (map/values f . ls)
  #;(assert (pair? ls))
  (let loop ((ls ls)
	     (result '()))
    (if (null? (car ls))
	(if (null? result)
	    '()
	    (unzip (reverse result)))
	(loop (map cdr ls)
	      `(,(call-with-values (lambda () (apply f (map car ls))) list)
		,@result)))))

(e.g.
 (map/values values '(1 2 3) '(a b c))
 ===> (1 2 3) (a b c))

(define (map-n n fn . lists)
  (define (map-n* lists)
    (if (any (is (length _) < n) lists)
	(values '() lists)
	(let* ((heads tails (map/values (lambda (l) (split-at l n)) lists))
	       (mapped (list<-values (apply fn (concatenate heads))))
	       (result rest (map-n* tails)))
	  (values
	   `(,@mapped . ,result)
	   rest))))
  (let ((result rest (map-n* lists)))
    (apply values result rest)))

(e.g.
 (map-n 2 + '(1 2 3 4 5))
 ===> (3 7) (5))

(e.g.
 (map-n 2 list '(1 2 3 4 5))
 ===> ((1 2) (3 4)) (5))

(e.g.
 (map-n 2 values '(1 2 3 4 5))
 ===> (1 2 3 4) (5))

(define (suffix? x y)
  (or (equal? x y)
      (and-let* (((h . t) y))
	(suffix? x t))))

(e.g. (suffix? '(c d e) '(a b c d e)))

(define (proper-suffix? x y)
  "is x a proper suffix of y?"
  (and (not (equal? x y))
       (suffix? x y)))

(e.g. 
 (not (proper-suffix? '(a b c) '(a b c))))

(define (prefix? x y)
  (or (null? x)
      (equal? x y)
      (and-let* (((hx . tx) x)
		 ((hy . ty) y)
		 ((equal? hx hy)))
	(prefix? tx ty))))

(e.g. (prefix? '(a b c) '(a b c d e)))

(define (proper-prefix? x y)
  "is x a proper prefix of y?"
  (and (not (equal? x y))
       (prefix? x y)))

(e.g.
 (not (proper-prefix? '(a b c) '(a b c))))

(define (find pred? . lsts)
  #;(assert (and (not (null? lsts))
	       (apply = (map length lsts))))
  (and-let* ((((heads . tails) ...) lsts))
    (if (apply pred? heads)
	(apply values heads)
	(apply find pred? tails))))

(define (take-right-while pred? list)
  (reverse (take-while pred? (reverse list))))

(define (drop-right-while pred? list)
  (reverse (drop-while pred? (reverse list))))

(define (take-upto n #;from l)
  (if (or (= n 0) (null? l))
      '()
      `(,(first l) . ,(take-upto (- n 1) (rest l)))))

(define (drop-upto n #;from l)
  (if (or (= n 0) (null? l))
      l
      (drop-upto (- n 1) (rest l))))

(define (weave . lists)
  (let ((lists (remove null? lists)))
    (if (null? lists)
	'()
	(let ((((heads . tails) ...) lists))
	  `(,@heads ,@(apply weave tails))))))

(e.g.
 (weave '(a b c) '(1 2 3) '(X Y Z))
 ===> (a 1 X b 2 Y c 3 Z))

(define (intersperse item #;into list)
  (match list
    (()
     '())
    ((last)
     list)
    ((first . rest)
     `(,first ,item . ,(intersperse item #;into rest)))))

(e.g.
 (intersperse '+ #;into '(1 2 3))
 ===> (1 + 2 + 3))

(define (chunks #;of l #;of-length n)
  (if (null? l)
      '()
      (let ((prefix rest (split-at l n)))
	`(,prefix . ,(chunks rest n)))))

(e.g.
 (chunks '(1 2 3 4 5 6) 3) ===> ((1 2 3) (4 5 6)))

(define (proper-list+dotted-tail improper-list)
  (match improper-list
    (`(,prefix . ,rest)
     (let ((proper-list tail (proper-list+dotted-tail rest)))
       (values `(,prefix . ,proper-list) tail)))
    (_
     (values '() improper-list))
    ))

(e.g.
 (proper-list+dotted-tail '(1 2 . 3)) ===> (1 2) 3)

(define (flatten x)
  (match x
    (`(,head . ,tail)
     `(,@(flatten head) ,@(flatten tail)))
    ('()
     '())
    (_
     `(,x))))

(e.g.
 (flatten '((1 . 2) 3 ((4)) . 5)) ===> (1 2 3 4 5))
