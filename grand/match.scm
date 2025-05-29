(define-module (grand match)
  #:use-module (grand mappings)
  #:export-syntax (match)
  #:export (actual-class)
  )

(define (actual-class type)
  type)

(define (match/equal? a b)
  (equal? a b))

(define-syntax match
  (syntax-rules ()
    ((match expression (pattern actions* ... value) ...)
     (let ((evaluated expression))
       (match/evaluated evaluated (pattern actions* ... value) ...)))))

(define-syntax match/evaluated
  (syntax-rules ()
    ((match/evaluated value)
     ;; This behavior is unspecified, and an "unspecified"
     ;; value would also be fine here.
     (error 'no-matching-pattern value))
    
    ((match/evaluated value (pattern actions ...) final-clause)
     (match-clause ((pattern value))
                   (and)
                   ()
                   actions ...
		   (match/evaluated value final-clause)))
    
    ((match/evaluated value (pattern actions ...) . clauses)
     (match-clause ((pattern value))
                   (and)
                   ()
                   actions ...
                   (match/evaluated value . clauses)))
    ))

(define-syntax match-clause
  (lambda (stx)
    (syntax-case stx (quasiquote
		      unquote quote unquote-splicing
		      and _ %typename)
      ((match-clause () condition bindings actions ... alternative)
       #'(check/unique condition bindings #f () ()
		       actions ... alternative))

      ((match-clause ((`,pattern root) . rest)
                     condition
                     bindings
                     actions ... alternative)
       #'(match-clause ((pattern root) . rest)
                       condition
                       bindings
                       actions ... alternative))

      ((match-clause ((,value root) . rest)
                     (conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       (conditions ... (match/equal? value root))
                       bindings
                       actions ... alternative))

      ((match-clause ((,@predicate root) . rest)
                     (conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       (conditions ... (predicate root))
                       bindings
                       actions ... alternative))
      
      ((match-clause ((_ root) . rest)
                     condition
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       condition
                       bindings
                       actions ... alternative))

      ((match-clause ((variable root) . rest)
                     condition
                     bindings
                     actions ... alternative)
       (identifier? #'variable)
       #'(match-clause rest
                       condition
                       ((variable root) . bindings)
                       actions ... alternative))

      ((match-clause (('datum root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       (and conditions ... (match/equal? root 'datum))
                       bindings
                       actions ... alternative))
      
      ((match-clause ((`(left . right) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause ((`left (car root)) (`right (cdr root)) . rest)
                       (and conditions ... (pair? root))
                       bindings
                       actions ... alternative))

      ((match-clause ((`datum root) . rest)
                     conditions
                     bindings
                     actions ... alternative)
       #'(match-clause (('datum root) . rest)
                        conditions
                       bindings
                       actions ... alternative))
      
      ((match-clause (((_ . fields) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause (((%typename . fields) root) . rest)
                       (and conditions ...)
                       bindings
                       actions ... alternative))
      
      ((match-clause (((%typename type) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       (and conditions ...)
                       bindings
                       actions ... alternative))

      ((match-clause (((%typename type key pat . etc) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       (and (keyword? (syntax->datum #'key))
	    (identifier? #'type))
       (with-syntax ((name (datum->syntax
			    stx
			    (keyword->symbol (syntax->datum #'key)))))
	 #'(match-clause (((%typename type . etc) root)
			  (pat (slot-ref root 'name)) . rest)
			 (and conditions ...)
			 bindings
			 actions ... alternative)))

      ((match-clause (((typename . fields) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       (identifier? #'typename)
       #'(match-clause (((%typename typename . fields) root) . rest)
                       (and conditions ... (instance? root (actual-class typename)))
                       bindings
                       actions ... alternative))

      ((match-clause ((literal root) . rest)
                     (and conditions ...)
                     bindings
                     actions ...)
       #'(match-clause rest
                       (and conditions ... (match/equal? literal root))
                       bindings
                       actions ...))
      )))

(define-syntax check/unique
  (lambda (stx)
    "add equality checks for repeated identifiers in patterns and remove them from bindings"
    (syntax-case stx (and)
      ((check/unique condition #;unchecked ()
		     #;currently-checked #f
		     #;checked ()
		     #;final bindings actions ... alternative)
       #'(if condition
             (let bindings actions ...)
             alternative))

      ;; check the next binding from the list
      ((check/unique condition
                     ((variable type ... path) . bindings)
                     #f
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique condition
                       bindings
                       (variable type ... path)
                       bindings/checked
                       bindings/final
                       actions ... alternative))

      ;; the binding is present: add equality check
      ((check/unique (and conditions ...)
                     ((variable type ... path) . bindings)
                     (variable+ type+ ... path+)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       (bound-identifier=? #'variable #'variable+)
       #'(check/unique (and conditions ... (match/equal? path path+))
                       bindings
                       (variable+ type+ ... path+)
                       bindings/checked
                       bindings/final
                       actions ... alternative))
      
      ;; the binding is absent: go on
      ((check/unique conditions
                     ((variable type ... path) . bindings)
                     (variable+ type+ ... path+)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique conditions
                       bindings
                       (variable+ type+ ... path+)
                       ((variable type ... path) . bindings/checked)
                       bindings/final
                       actions ... alternative))

      ;; add binding to the "checked" list
      ;; (and possibly start over)
      ((check/unique conditions
                     ()
                     (variable type ... path)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique conditions
                       bindings/checked
		       #f
		       ()
                       ((variable type ... path) . bindings/final)
                       actions ... alternative))
      )))
