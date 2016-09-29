(define-module (grand define-memoized)
  #:use-module (grand syntax)
  #:use-module (srfi srfi-1)
  #:export (memoize define/memoized))


(define (memoize proc)
  (let ((cache (make-hash-table)))
    (lambda args
      (match (hash-get-handle cache args)
	((key . memoized-result)
	 (apply values memoized-result))
	(_
	 (call-with-values (lambda () (apply proc args))
	   (lambda result
	     (hash-set! cache args result)
	     (apply values result))))))))

(define-syntax (define/memoized (name . args) . body)
  (define name (memoize (lambda args . body))))
