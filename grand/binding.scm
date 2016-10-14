(define-module (grand binding)
  #:use-module (grand syntax)
  #:use-module (grand examples)
  #:export (bind))

(define (bind pattern #;to form)
  (define (fit pattern #;to form #;with bound-variables)
    (match pattern
      ('_ bound-variables)
      (('quote item)
       (and (equal? item form)
	    bound-variables))
      ((head/pattern . tail/pattern)
       (and-let* (((head/form . tail/form) form)
		  (bound* (fit head/pattern #;to head/form
			       #;with bound-variables)))
	 (fit tail/pattern #;to tail/form #;with bound*)))
      ((? symbol?)
       (cond ((assoc pattern bound-variables)
	      => (lambda ((key . value))
		   (and (equal? value form) bound-variables)))
	     (else
	      `((,pattern . ,form) . ,bound-variables))))
      (_
       (and (equal? pattern form)
	    bound-variables))))
  (fit pattern #;to form #;with '()))

(e.g. (bind '(a b . c) #;to '(1 2 3)) ===> ((c 3) (b . 2) (a . 1)))
