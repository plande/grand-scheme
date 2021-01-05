(define-module (grand define-keywords)
  #:use-module (ice-9 match)
  #:replace ((lambda/kw . lambda*)
	     (define/kw . define*)))


(define (keyword-arg? x)
  (and (keyword? x)
       (not (eq? x #:=))))

(define (remove-attributes attributes #;from attribute-list)
  (match attribute-list
    (((? keyword? key) value . rest)
     (if (member key attributes)
	 (remove-attributes attributes rest)
	 `(,key ,value . ,(remove-attributes
			   attributes #;from rest))))
    (_
     attribute-list)))

(define-syntax-rule (lambda/kw args . body)
  (process-lambda/kw args #;req () #;opt () #;kw ()
		     #;destruct () body))

(define-syntax process-lambda/kw
  (lambda (stx)
    (syntax-case stx ()

      ((_ () (req ...) (opt ...) (kw ...) (destruct ...) body)
       #'(lambda* (req ... #:optional opt ... #:key kw ...)
	 (match-let (destruct ...) . body)))

      ((_ last-tail (req ...) (opt ...)
	  ((name default-value key) ...) (destruct ...) body)
       (identifier? #'last-tail)
       #'(lambda* (req ... #:optional opt ... #:key
		       (name default-value key) ...
		       #:allow-other-keys . last-tail)
	   (match-let ((last-tail (remove-attributes
				   '(key ...) last-tail))
		       destruct ...) . body)))
      
      ((_ (key name #:= val . rest) req
	  opt (kw ...) destruct body)
       (and (keyword-arg? (syntax->datum #'key))
	    (identifier? #'name))
       #'(process-lambda/kw rest req opt
			    (kw ... (name val key))
			    destruct body))

      ((_ (key pat #:= val . rest) req
	  opt (kw ...) (destruct ...) body)
       (and (keyword-arg? (syntax->datum #'key))
	    (not (keyword? (syntax->datum #'pat))))
       #'(process-lambda/kw rest req opt
			    (kw ... (name val key))
			    (destruct ... (pat name)) body))

      ((_ (name #:= val . rest) req (opt ...) kw
	  destruct body)
       (identifier? #'name)
       #'(process-lambda/kw rest req (opt ... (name val))
			    kw destruct body))

      ((_ (pat #:= val . rest) req (opt ...) kw
	  (destruct ...) body)
       (not (keyword? (syntax->datum #'pat)))
       #'(process-lambda/kw rest req (opt ... (name val))
			    kw (destruct ... (pat name)) body))
      
      ((_ (key name . rest) req
	  opt (kw ...) destruct body)
       (and (keyword-arg? (syntax->datum #'key))
	    (identifier? #'name))
       #'(process-lambda/kw rest req opt
			    (kw ... (name #f key))
			    destruct body))

      ((_ (key pat . rest) req
	  opt (kw ...) (destruct ...) body)
       (and (keyword-arg? (syntax->datum #'key))
	    (not (keyword? (syntax->datum #'pat))))
       #'(process-lambda/kw rest req opt
			    (kw ... (name #f key))
			    (destruct ... (pat name)) body))
      
      ((_ (name . rest) (req ...)
	  opt kw destruct body)
       (identifier? #'name)
       #'(process-lambda/kw rest (req ... name) opt kw
			    destruct body))

      ((_ (pat . rest) (req ...)
	  opt kw (destruct ...) body)
       (not (keyword? (syntax->datum #'pat)))
       #'(process-lambda/kw rest (req ... name) opt kw
			    (destruct ... (pat name)) body))

      )))

(define-syntax-rule (define/kw (name . args) . body)
  (define name (lambda/kw args . body)))
