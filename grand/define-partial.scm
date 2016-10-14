(define-module (grand define-partial)
  #:use-module (grand syntax)
  #:use-module (grand binding)
  #:use-module (srfi srfi-1)
  #:use-module (grand examples)
  #:export (define/partial))

(define (specializes? pattern-a pattern-b)
  "is pattern-a a specialization of pattern-b?"
  (and-let* ((bindings (bind pattern-b #;to pattern-a)))
    (any (lambda ((key . value))
	   (or (not (symbol? value))
	       (any (lambda ((other-key . other-value))
		      (and (not (equal? key other-key))
			   (equal? value other-value)))
		    bindings)))
	 bindings)))

(define (partial-procedure? x)
  (and (procedure? x)
       (procedure-property x 'instances)))

(define-syntax (impose-properties ((property value) ...) procedure)
  (let ((proc procedure))
    (set-procedure-property! proc 'property value) ...
    proc))

(define (make-partial procedure-name)
  (define (dispatch . args)
    (cond ((find (lambda ((pattern . procedure))
		   (apply pattern args))
		 (procedure-property dispatch 'instances))
	   => (lambda ((pattern . procedure))
		(apply procedure args)))
	  (else
	   (throw 'no-matching-instance procedure-name args))))
  (impose-properties ((name procedure-name)(instances '())) dispatch))

(define (less-general-pattern? (guard-1 . _) (guard-2 . _))
  "is the pattern of guard-1 less general than that of guard-2?"
  (let ((pattern-1 (procedure-property guard-1 'pattern))
	(pattern-2 (procedure-property guard-2 'pattern)))
    (specializes? pattern-1 pattern-2)))

(define (equivalent-patterns? guard-1 guard-2)
  (let ((pattern-1 (procedure-property guard-1 'pattern))
	(pattern-2 (procedure-property guard-2 'pattern)))
    (and-let* ((fit-1 (bind pattern-1 pattern-2))
	       (fit-2 (bind pattern-2 pattern-1)))
      (equal? fit-1 (map (lambda ((k . v)) `(,v . ,k)) fit-2)))))

(define (extend-partial partial pattern instance)
  (let* ((instances (procedure-property partial 'instances))
	 (new-instances (if (any (lambda ((existing-pattern . _))
				   (equivalent-patterns? existing-pattern
							 pattern))
				 instances)
			    (map (lambda ((guard . action))
				   (if (equivalent-patterns? pattern
							     guard)
				       (begin
					 (warn
					  'replacing
					  (procedure-property guard
							      'pattern)
					  'with
					  (procedure-property pattern
							      'pattern))
					 `(,pattern . ,instance))
				       `(,guard . ,action)))
				 instances)
			    (merge `((,pattern . ,instance))
				   instances
				   less-general-pattern?))))
    (impose-properties ((instances new-instances)) partial)))

(define (numbered-symbol prefix number)
  (symbol-append prefix (string->symbol (number->string number))))

(define (argument-list arity variadic?)
  (if (zero? arity)
      (if variadic?
	  'rest
	  '())
      `(,(numbered-symbol 'arg- arity)
	. ,(argument-list (- arity 1) variadic?))))

(define (procedure->instance proc)
  (let* (((arity _ variadic?) (procedure-minimum-arity (lambda (x) (+ x x))))
	 (guard (argument-list arity variadic?)))
    (extend-partial
     (make-partial (procedure-name proc))
     (impose-properties ((pattern guard))
			(lambda args
			  ((if variadic? >= =) (length args) arity)))
     proc)))

(define-syntax (define/partial (f . args) body . *)
  (define f
    (extend-partial
     (if (and (defined? 'f) (procedure? f))
	 (if (partial-procedure? f)
	     f
	     (procedure->instance f))
	 (make-partial 'f))
     (impose-properties ((pattern 'args))
         (lambda args* (and-let* ((args args*)))))
     (lambda args body . *))))
