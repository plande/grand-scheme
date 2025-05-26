(define-module (grand mappings)
  #:use-module (srfi srfi-17)
  #:export-syntax (mapping
		   attribute
		   attribute+
		   define-mapping
		   define-attribute
		   define-attribute+
		   unset!)
  #:export (keys)
  )

(define-syntax mapping
  (syntax-rules ()
    ((mapping (object) default)
     (let* ((table (make-hash-table))
	    (not-found (gensym "not-found"))
	    (accessor (make-procedure-with-setter
			  (lambda (object)
			    (let ((value (hash-ref table object not-found)))
			      (if (eq? value not-found)
				  default
				  value)))
			  (lambda (arg value)
			    (hash-set! table arg value)))))
       (set-procedure-property! accessor 'table table)
       accessor))))

(define-syntax attribute
  (syntax-rules ()
    ((attribute (object) default)
     (let* ((table (make-weak-key-hash-table))
	    (not-found (gensym "not-found"))
	    (accessor (make-procedure-with-setter
			  (lambda (object)
			    (let ((value (hash-ref table object not-found)))
			      (if (eq? value not-found)
				  default
				  value)))
			  (lambda (arg value)
			    (hash-set! table arg value)))))
       (set-procedure-property! accessor 'table table)
       accessor))))

(define-syntax attribute+
  (syntax-rules ()
    ((attribute (object) default)
     (let* ((table (make-weak-key-hash-table))
	    (not-found (gensym "not-found"))
	    (accessor (make-procedure-with-setter
			  (lambda (object)
			    (let ((value (hash-ref table object not-found)))
			      (if (eq? value not-found)
				  (let ((value default))
				    (hash-set! table object value)
				    value)
				  value)))
			  (lambda (arg value)
			    (hash-set! table arg value)))))
       (set-procedure-property! accessor 'table table)
       accessor))))

(define-syntax unset!
  (syntax-rules ()
    ((unset! attribute key)
     (let* ((table (procedure-property attribute 'table)))
       (hash-remove! table key)))
    ((unset! (attribute key))
     (unset! attribute key))))

(define-syntax define-mapping
  (syntax-rules ()
    ((define-mapping (mapping-name object) default)
     (define mapping-name
       (mapping (object) default)))))

(define-syntax define-attribute
  (syntax-rules ()
    ((define-attribute (attribute-name object) default)
     (define attribute-name
       (attribute (object) default)))))

(define-syntax define-attribute+
  (syntax-rules ()
    ((define-attribute+ (attribute-name object) default)
     (define attribute-name
       (attribute+ (object) default)))))

(define (keys dict)
  (let ((table (procedure-property dict 'table)))
    (hash-map->list (lambda (key value) key) table)))
