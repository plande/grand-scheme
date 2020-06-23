(define-module (grand function)
  #:use-module (grand examples)
  #:use-module (grand syntax)
  #:use-module (srfi srfi-1)
  #:export (impose-arity
	    arity
	    name/source
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
	    isnt))

(use-modules (grand examples) (grand syntax) (srfi srfi-1))

(define (impose-arity n procedure)
  (let ((new-procedure (lambda args (apply procedure args))))
    (set-procedure-property! new-procedure 'name
			     (or (procedure-name procedure)
				 'fixed-arity))
    (set-procedure-property! new-procedure 'imposed-arity
			     (if (list? n) n `(,n 0 #f)))
    new-procedure))

(define (name/source procedure)
  (or (procedure-name procedure)
      (procedure-source procedure)))

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
  (define (make-chain chains fn)
    (impose-arity
     (arity fn)
     (lambda args
       (call-with-values 
	   (lambda () (apply fn args))
	 (lambda vals (apply chains (clip vals (arity chains))))))))
  (let ((composer (fold-right make-chain values fns)))
    composer))

(define (iterations n f)
  (apply compose/values (make-list n f)))

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


(define-syntax infix/postfix ()
  
  ((infix/postfix x somewhat?)
   (somewhat? x))

  ((infix/postfix left related-to? right)
   (related-to? left right))

  ((infix/postfix left related-to? right . likewise)
   (let ((right* right))
     (and (infix/postfix left related-to? right*)
	  (infix/postfix right* . likewise)))))

(define-syntax extract-placeholders (_ is isnt)
  ((extract-placeholders final (() () body))
   (final (infix/postfix . body)))

  ((extract-placeholders final (() args body))
   (lambda args (final (infix/postfix . body))))

  ((extract-placeholders final (((is . t) . rest) args (body ...)) . *)
   (extract-placeholders final (rest args (body ... (is . t))) . *))

  ((extract-placeholders final (((isnt . t) . rest) args (body ...)) . *)
   (extract-placeholders final (rest args (body ... (isnt . t))) . *))

  ((extract-placeholders final (((h . t) . rest) args body) . *)
   (extract-placeholders final ((h . t) () ()) (rest args body) . *))

  ((extract-placeholders final (() (args ...) body) (rest (args+ ...)
                                                          (body+ ...)) . *)
   (extract-placeholders final (rest (args+ ... args ...)
                                     (body+ ... body)) . *))

  ((extract-placeholders final ((_ . rest) (args ...) (body ...)) . *)
   (extract-placeholders final (rest (args ... arg) (body ... arg)) . *))

  ((extract-placeholders final ((arg . rest) args (body ...)) . *)
   (extract-placeholders final (rest args (body ... arg)) . *))

  )

(define-syntax (identity-syntax form)
  form)

(define-syntax (is . something)
  (extract-placeholders identity-syntax (something () ())))

(define-syntax (isnt . something)
  (extract-placeholders not (something () ())))

(e.g.
 (is 2 < 3))

(e.g.
 (is 1 < 2 = (+ 1 1) <= 3 odd?))

(e.g.
 (filter (is 5 < _ <= 10) '(1 3 5 7 9 11))
 ===> (7 9))

(e.g.
 (let ((<* (is (length _) < (length _))))
   (is '(1 2 3) <* '(1 2 3 4))))

(e.g.
 ((is (length (filter (is (modulo _ 2) = 0) _)) < 5)
  '(1 2 3 4 5 6 7 8 9)))

(e.g.
 (is 2 even?))

(e.g.
 (isnt 2 odd?))

(e.g.
 (every (is (+ _ _) even?) '(1 2 3 4) '(5 6 7 8)))

(e.g.
 ((is 2 _ 3) <))

(e.g.
 ((is _ _ _) 2 < 3))

(e.g.
 ((is (expt _ 2) _ (expt _ 2) _ (expt _ 2)) 2 <= -2 < -3))
