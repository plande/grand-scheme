(define-module (grand examples)
  #:export (e.g.))

(define-syntax e.g.
  (syntax-rules (===> ~~~> ===>!)
    ((_ example)
     (or example
	 (error 'example)))
    ((_ example ===> value)
     (let ((result example))
       (if (equal? result 'value)
	   result
	   (error '(example ===> value) result))))

    ((_ example ===> value ...)
     (call-with-values (lambda () example)
       (lambda results
	 (if (equal? results '(value ...))
	     (apply values results)
	     (error '(example ===> value ...) results)))))

    ((_ example ===>! errors ...)
     (let ((result (catch #t (lambda () example #f) list)))
       (or result
	   (error 'example " failed to raise error "
		  'errors ...))))

    
    ((_ example ~~~> value) ;; the ~~~> reads as "may nondeterministically
     example))) ;; evaluate to"

