(define-module (grand define-keywords)
  #:use-module (grand syntax)
  #:use-module (grand attributes)
  #:use-module (grand list)
  #:export (define/keywords))

(define-syntax define/keywords
  (lambda (x)

    (define* (required args #:optional (gathered '()))
      (match args
	(((? symbol?) #:= . _)
	 (values (reverse gathered) args))
	(((? symbol? s) . rest)
	 (required rest `(,s . ,gathered)))
	(_
	 (values (reverse gathered) args))))

    (define* (optional args #:optional (gathered '()))
      (match args
	(((? symbol? s) #:= value . rest)
	 (optional rest `((,s ,value) . ,gathered)))
	(_
	 (values (reverse gathered) args))))

    (define* (keyword args #:optional (gathered '()))
      (match args
	(((? keyword? k) (? symbol? s) #:= value . rest)
	 (keyword rest `((,s ,value ,k) . ,gathered)))
	(((? keyword? k) (? symbol? s) . rest)
	 (keyword rest `((,s #f ,k) . ,gathered)))
	(_
	 (values (reverse gathered) args))))

    (define (required+optional+keyword+rest+keys args)
      (let* ((required args* (required (syntax->datum args)))
	     (optional args** (optional args*))
	     (keyword rest (keyword args**))
	     (((names values keys) ...) keyword))
	(datum->syntax x `(,required ,optional ,keyword ,rest ,keys))))

    (syntax-case x ()

      ((_ (proc args ...) body ...)
       (with-syntax ((((required ...) (optional ...) (keyword ...) () keys)
		      (required+optional+keyword+rest+keys #'(args ...))))
	 #'(define proc
	     (lambda* (required ... #:optional optional ...
				#:key keyword ...
				#:allow-other-keys)
	       body ...))))

      ((_ (proc . args) body ...)
       (with-syntax ((((required ...) (optional ...) (keyword ...) rest keys)
		      (required+optional+keyword+rest+keys #'args)))
	 #'(define proc
	     (lambda* (required ... #:optional optional ...
				#:key keyword ...
				#:allow-other-keys . rest)
	       (let ((rest (remove-attributes 'keys rest)))
		 body ...))))))))
