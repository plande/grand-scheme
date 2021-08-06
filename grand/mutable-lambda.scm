(define-module (grand mutable-lambda)
  #:use-module (srfi srfi-1) ; fold, every, last, drop-right
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-17) ; getter-with-setter
  #:use-module (srfi srfi-39) ; parameter objects
  #:use-module (srfi srfi-69) ; hash-table
  #:export (lambda!-equal? lambda!-hash)
  #:export-syntax (lambda! define-lambda!))

(define (list-equal? equal?)
  (define (equal-lists? . lists)
    (or (every null? lists)
	(and (every pair? lists)
	     (apply equal? (map car lists))
	     (apply equal-lists? (map cdr lists)))))
  equal-lists?)


(define (list-hash custom-hash)
  (lambda (list . size?)
    (fold (lambda (x h)
	    (apply hash (+ h (custom-hash x)) size?))
	  (apply hash list size?)
	  list)))

(define lambda!-equal?
  (make-parameter equal?))

(define lambda!-hash
  (make-parameter hash))

(define mutable-procedure-override
  (make-hash-table))

(define-syntax-rule (lambda! args . body)
  (let* ((override (make-hash-table
		    (list-equal? (lambda!-equal?))
		    (list-hash (lambda!-hash))))
	 (default (lambda args . body))
	 (getter (lambda args*
		  (if (hash-table-exists? override args*)
		      (hash-table-ref override args*)
		      (apply default args*))))
	 (setter (lambda args+value
		   (hash-table-set! override
				    (drop-right args+value 1)
				    (last args+value))))
	 (proc (getter-with-setter getter setter)))
    (hash-table-set! mutable-procedure-override
		     proc override)
    (hash-table-set! mutable-procedure-override
		     getter override)
    (hash-table-set! mutable-procedure-override
		     setter override)
    proc))

(define (extended-domain procedure)
  (and-let* ((override (hash-table-ref
			mutable-procedure-override
			procedure #f)))
    (hash-table-keys override)))

(define-syntax-rule (lambdaq args . body)
  (parameterize ((lambda!-equal? eq?)
		 (lambda!-hash hashq))
    (lambda! args . body)))

(define-syntax-rule (lambdav args . body)
  (parameterize ((lambda!-equal? eqv?)
		 (lambda!-hash hashv))
    (lambda! args . body)))

(define-syntax-rule (lambda? args . body)
  (parameterize ((lambda!-equal? equal?)
		 (lambda!-hash hash))
    (lambda! args . body)))

(define-syntax-rule (define-lambda! (prototype . args)
		      . body)
  (define prototype
    (let ((f (lambda! args . body)))
      (set-procedure-property! f 'name 'prototype)
      f)))
