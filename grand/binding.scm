(define-module (grand binding)
  #:use-module (grand syntax)
  #:use-module (grand examples)
  #:export (bind fill))

(define (...? x)
  (eq? x '...))

(define (zip-bindings list-of-bindings)
  (match list-of-bindings
    ((((names . values) ...) ...)
     (match names
       ((names . _)
	(apply map list names values))
       (()
	'())))))

(e.g.
 (zip-bindings '(((a . 1) (b . 2) (c . 3))
		 ((a . 4) (b . 5) (c . 6))
		 ((a . 7) (b . 8) (c . 9))))
 ===> '((a 1 4 7) (b 2 5 8) (c 3 6 9)))

(define (union/bindings a b)
  (fold-left (lambda (bindings (name . value))
	       (and bindings
		    (match (assoc name bindings)
		      ((name . value*)
		       (and (equal? value value*)
			    bindings))
		      (_
		       `((,name . ,value) . ,bindings)))))
	     a
	     b))

(define (carry #;from prefix #;to suffix #;until condition)
  (let ((result (condition prefix suffix)))
    (if (or result (null? prefix))
	result
	(let (((initial ... last) prefix))
	  (carry #;from initial #;to `(,last . ,suffix)
			#;until condition)))))

(define (bind pattern #;to form)
  (define (fit pattern #;to form #;with bound-variables)
    (match pattern
      ((compound (? ...?) . remainder)
       (let* ((matching rest (span (lambda (constituent)
				     (bind compound constituent))
				   form)))
	 (carry #;from matching #;to rest #;until
		       (lambda (matching rest)
			 (let* ((matching (map (lambda (constituent)
						 (bind compound constituent))
					       matching))
				(merged (union/bindings
					 bound-variables
					 (zip-bindings matching))))
			   (and merged (fit remainder #;to rest
					    #;with merged)))))))
      (('quote item)
       (and (equal? item form)
	    bound-variables))
      
      ((head/pattern . tail/pattern)
       (and-let* (((head/form . tail/form) form)
		  (bound* (fit head/pattern #;to head/form
			       #;with bound-variables)))
	 (fit tail/pattern #;to tail/form #;with bound*)))
      (_
       (cond ((not (symbol? pattern))
	      (and (equal? pattern form)
		   bound-variables))
	     ((eq? pattern '_)
	      bound-variables)
	     ((assoc pattern bound-variables)
	      => (lambda ((key . value))
		   (and (equal? value form) bound-variables)))
	     (else
	      `((,pattern . ,form) . ,bound-variables))))
      ))
  (fit pattern #;to form #;with '()))

(e.g. (bind '(a b . c) #;to '(1 2 3)) ===> ((c 3) (b . 2) (a . 1)))

#;(bind '('let ((name value) ...) . body)
'(let ((x 5) (y 10) (z 15))
(+ x y)))

(define (symbols x)
  (match x
    (('quote _)
     '())
    ((h . t)
     (union (symbols h) (symbols t)))
    (_
     (if (symbol? x)
	 `(,x)
	 '()))))

(define (unzip-bindings #;of symbols #;in bindings)
  (let* ((zipped remaining (partition (lambda ((name . value))
					(is name member #;of symbols))
				      bindings))
	 (((names . values) ...) zipped))
    (map (lambda (value)
	   `(,@(map cons names value) . ,remaining))
	 (transpose values))))

(e.g.
 (unzip-bindings '(a b) '((a 1 2 3) (b 4 5 6) (c . 3)))
 ===> (((a . 1) (b . 4) (c . 3))
       ((a . 2) (b . 5) (c . 3))
       ((a . 3) (b . 6) (c . 3))))

(define (fill template #;with bindings)
  (match template
    ((compound (? ...?) . rest)
     (let* ((symbols (symbols compound))
	    (unzipped (unzip-bindings #;of symbols #;in bindings)))
       `(,@(map (lambda (subbindings)
		  (fill compound #;with subbindings))
		unzipped) . ,(fill rest #;with bindings))))
    (('quote item)
     item)

    ((head . tail)
     `(,(fill head #;with bindings) . ,(fill tail #;with bindings)))
    
    (_
     (cond ((assoc template bindings)
	    => (lambda ((key . value))
		 value))
	   (else
	    template)))))

(e.g.
 (fill '(let ((name value) ...) . body)
       '((name a b) (value 5 10) (body (+ a b))))
 ===> (let ((a 5) (b 10))
	(+ a b)))

#;(assert (lambda (template form)
	  (equal? (fill template (bind template form)) form)))
