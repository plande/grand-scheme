(define-module (grand function)
  #:use-module (grand examples)
  #:use-module (grand syntax)
  #:use-module (srfi srfi-1)
  #:export (impose-arity
	    arity
	    clip
	    pass
	    compose/values
	    iterations
	    partial
	    maybe
	    either
	    neither
	    both
	    is
	    isn't))

(define (impose-arity n procedure)
  (let ((new-procedure (lambda args (apply procedure args))))
    (set-procedure-property! new-procedure 'name
			     (or (procedure-name procedure)
				 'fixed-arity))
    (set-procedure-property! new-procedure 'imposed-arity
			     (if (list? n) n `(,n 0 #f)))
    new-procedure))

(define (arity procedure)
  ;;(assert (procedure? procedure))
  (or (procedure-property procedure 'imposed-arity)
      (procedure-property procedure 'arity)))

(define (clip args #;to arity)
  (match arity
    ((min _ #f)
     (take args min))
    ((? number?)
     (take args arity))
    (_
     args)))

(define (compose/values . fns)
  (define (make-chain fn chains)
    (impose-arity
     (arity fn)
     (lambda args
       (call-with-values 
	   (lambda () (apply fn args))
	 (lambda vals (apply chains (clip vals (arity chains))))))))
  (let ((composer (reduce make-chain values fns)))
    composer))

(define (iterations n f)
  (apply compose (make-list n f)))

(e.g.
 ((iterations 3 1+) 0)
 ===> 3)

(define (pass object #;to . functions)
  ((apply compose/values (reverse functions)) object))

(e.g. (pass 5 #;to 1- #;to sqrt) ===> 2)

(define ((partial function . args) . remaining-args)
  (apply function `(,@args ,@remaining-args)))
  
#;(assert (lambda (f x)
(if (defined? (f x))
    (equal? (f x) ((partial f) x)))))

(define ((maybe pred) x)
  (or (not x)
      (pred x)))

(e.g.
 (and ((maybe number?) 5)
      ((maybe number?) #f)))

(define ((either . preds) x)
  (any (lambda (pred)
	 (pred x))
       preds))

(define ((neither . preds) x)
  (not ((apply either preds) x)))

(e.g.
 (and ((either number? symbol?) 5)
      ((either number? symbol?) 'x)
      ((neither number? symbol?) "abc")))

(define ((both . preds) x)
  (every (lambda (pred)
	   (pred x))
	 preds))

(e.g.
 (and ((both positive? integer?) 5)
      (not ((both positive? integer?) 4.5))))

(define (is . stuff)
  (match stuff
    ((x related-to? y)
     (related-to? x y))
    ((x related-to? y . likewise)
     (and (related-to? x y)
	  (apply is y likewise)))))

(define (isn't . stuff)
  (not (apply is stuff)))

