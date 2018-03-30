(define-module (grand define-memoized)
  #:use-module (grand syntax)
  #:use-module (srfi srfi-1)
  #:use-module (grand function)
  #:export (memoize define/memoized))

(define (memoize proc)
  (let ((cache (make-hash-table)))
    (with-procedure-properties ((memoization-table cache))
      (lambda args
	(match (hash-get-handle cache args)
	  ((key . memoized-result)
	   (apply values memoized-result))
	  (_
	   (call-with-values (lambda () (apply proc args))
	     (lambda result
	       (hash-set! cache args result)
	       (apply values result)))))))))


(define-syntax (define/memoized (name . args) . body)
  (define name (memoize (lambda args . body))))

(define/memoized (edit-distance a b)
  ;; Levinshtein distance
  (match `(,a ,b)
    (`(,a ())
     (length a))
    (`(() ,b)
     (length b))
    (`((,a0 . ,a*) (,b0 . ,b*))
     (min (+ (edit-distance a* b) 1)
	  (+ (edit-distance a b*) 1)
	  (+ (edit-distance a* b*)
	     (if (equal? a0 b0) 0 2))))))
