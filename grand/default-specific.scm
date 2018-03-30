(define-module (grand default-specific)
  #:use-module (grand syntax)
  #:use-module (srfi srfi-1)
  #:use-module (grand examples)
  #:use-module (grand list)
  #:export (with-default without-default specify))

(define SPECIFIC-CONTEXT (make-hash-table))

(let-syntax (((specific-literal-syntax binding-structure name value)
	      (lambda (stx)
		;;(assert (appears? #'name #;in #'binding-structure))
		(syntax-case stx ()
		  ((_ (binding-structure (... ...)) actions . *)
		   (with-syntax ((specific (datum->syntax stx 
							  'specific)))
		     #'(let-syntax 
			   ((specific
			     (syntax-rules (name (... ...))
			       ((_ name)
				(let ((default (hash-ref
						 SPECIFIC-CONTEXT
						'name '())))
				  (if (null? default)
				      value
				      (first default))))
			       (... ...))))
			 actions . *)))))))
  (define-syntax with-default 
    (specific-literal-syntax (name value) name value))
  (define-syntax without-default 
    (specific-literal-syntax 
     name name (throw 'unspecified #:name 'name
		      #:context (hash-map->list cons SPECIFIC-CONTEXT)))))

(define-syntax specify
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((name expression) ...)
	  actions ...)
       (with-syntax (((value ...) (generate-temporaries 
				   #'(expression ...))))
	 #'(let ((value expression) ...)
	     (dynamic-wind
	       (lambda ()
		 (hash-set! SPECIFIC-CONTEXT 'name
			    (cons value (hash-ref SPECIFIC-CONTEXT 
						  'name '())))
		 ...)
	       (lambda ()
		 actions ...)
	       (lambda ()
		 (hash-set! SPECIFIC-CONTEXT 'name
			    (rest (hash-ref SPECIFIC-CONTEXT 'name)))
		 ...))))))))

(e.g.                   ; this is how the trio 'with-default',
 (let ()                ; 'specific' and 'specify' can be used
   (with-default ((x 10)
		  (y 20))
     (define (f)
       `(,(specific x) ,(specific y))))
   (specify ((x 30))
     (f)))
 ===> (30 20))
