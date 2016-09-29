(define-module (grand examples)
  #:use-module (grand syntax)
  #:use-module (srfi srfi-1)
  #:export (e.g.))

(define-syntax e.g. (===> ~~~>)
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

  ((_ example ~~~> value) ;; the ~~~> reads as "may nondeterministically
   example)) ;; evaluate to"
