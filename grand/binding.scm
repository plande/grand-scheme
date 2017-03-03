(define-module (grand binding)
  #:use-module (grand syntax)
  #:use-module (grand examples)
  #:use-module (grand function)
  #:use-module (grand matrix)
  #:use-module (grand set)
  #:use-module (grand list)
  #:export (bind fill))

(define (...? x)
  (eq? x '...))

(define (unique-symbol base)
  (cond ((string? base)
	 (make-symbol base))
	((symbol? base)
	 (make-symbol (symbol->string base)))))

(define (prefix-length pred l)
  (define (traverse l n)
    (or (and-let* (((h . t) l)
		   ((pred h)))
	  (traverse t (+ n 1)))
	n))
  (traverse l 0))

(e.g.
 (prefix-length even? '(2 4 6 7 8 9)) ===> 3)

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
 ===> ((a 1 4 7) (b 2 5 8) (c 3 6 9)))

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

(define (carry #;from prefix #;to suffix #;until success?)
  (let ((result (success? prefix suffix)))
    (if (or result (null? prefix))
	result
	(let (((initial ... last) prefix))
	  (carry #;from initial #;to `(,last . ,suffix)
			#;until condition)))))

(define (rename-clashing expression)
  
  (define (rename-symbols expression replacements)
    (match expression
      (('quote _)
       (values expression replacements))

      ((unzipped (? ...?) . rest)
       (let* ((unzipped replacements (rename-symbols unzipped replacements))
	      (ellipses (prefix-length ...? rest))
	      (...* rest (split-at rest ellipses))
	      (rest replacements (rename-symbols rest replacements)))
	 (values `(,unzipped ... ,@...* . ,rest) replacements)))

      ((head . tail)
       (let* ((head replacements (rename-symbols head replacements))
	      (tail replacements (rename-symbols tail replacements)))
	 (values `(,head . ,tail) replacements)))
      
      (_
       (cond ((not (symbol? expression))
	      (values expression replacements))
	     ((find (lambda ((replacement . original))
		      (eq? expression original))
		    replacements)
	      => (lambda ((replacement . original))
		   (values replacement replacements)))
	     (else
	      (let ((replacement (unique-symbol expression)))
		(values replacement `((,replacement . ,expression)
				      . ,replacements))))))))
  
  (define (rename-clashing expression replacements)
    (match expression
      (('quote _)
       (values expression replacements))

      ((unzipped (? ...?) . rest)
       (let* ((unzipped replacements (rename-symbols unzipped replacements))
	      (unzipped replacements (rename-clashing unzipped replacements))
	      (ellipses (prefix-length ...? rest))
	      (...* rest (split-at rest ellipses))
	      (rest replacements (rename-clashing rest replacements)))
	 (values `(,unzipped ... ,@...* . ,rest) replacements)))

      ((head . tail)
       (let* ((head replacements (rename-clashing head replacements))
	      (tail replacements (rename-clashing tail replacements)))
	 (values `(,head . ,tail) replacements)))

      (_
       (values expression replacements))))

  (rename-clashing expression '()))

(and-let* (((a . b) (c . d) (values '(1 . 2) '(3 . 4))))
  (list a b c d))

(e.g.
 (and-let* ((((a* (a** (? ...?)) a*) (? ...?)) ((a** . a*)(a* . a))
	      (rename-clashing '((a (a ...) a) ...))))))

'((a** . a*) (a* . a))

(define (fold-bindings bindings mapping)
  (match mapping
    (()
     bindings)
    (((renamed . original) . remaining-mappings)
     (let ((unzipped (assoc renamed bindings))
	   (zipped (assoc original bindings)))
       (cond ((and unzipped zipped)
	      (let (((renamed . unzipped) unzipped)
		    ((original . zipped) zipped))
		(and (every (lambda (repetition)
			      (equal? repetition zipped))
			    unzipped)
		     (fold-bindings (filter (lambda ((name . value))
					      (not (equal? name renamed)))
					    bindings)
				    remaining-mappings))))
	     (else
	      (and bindings
		   (fold-bindings (map (lambda ((name . value))
					 (if (equal? name renamed)
					     `(,original . ,value)
					     `(,name . ,value)))
				       bindings)
				  remaining-mappings))))))))

(e.g.
 (fold-bindings '((a** ((1 2 3) (1 2 3) (1 2 3))
		       ((1 2 3) (1 2 3) (1 2 3))
		       ((1 2 3) (1 2 3) (1 2 3)))
		  (a* (1 2 3) (1 2 3) (1 2 3))
		  (a 1 2 3))
		'((a** . a*)(a* . a+)(a+ . a)))
 ===> ((a 1 2 3)))

(define (bind pattern #;to form)
  (define (fit pattern #;to form #;with bound-variables)
    (match pattern
      ;; note: because of a bug in Shinn's matcher,
      ;; we need to write (? ...?) instead of '...
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
  (let* ((pattern mapping (rename-clashing pattern))
	 (bindings (fit pattern #;to form #;with '())))
    (fold-bindings bindings #;with mapping)))

(e.g. (bind '(a b . c) '(1 2 3)) ===> ((c 3) (b . 2) (a . 1)))

(e.g.
 (bind '((a (a ...) a) ...) '((1 (1 2 3) 1)
			      (2 (1 2 3) 2)
			      (3 (1 2 3) 3)))
 ===> ((a 1 2 3)))
 
(define (replace-unzipped #;in expression #;with replacements)
  (match expression
    (('quote _)
     expression)

    ((zipped (? ...?) . rest)
     (let* ((n (prefix-length ...? rest))
	    (rest (drop rest n)))
       `(,zipped ,@((iterations n (partial cons '...)) '(...))
		 ,@(replace-unzipped #;in rest #;with replacements))))
    
    ((head . tail)
     `(,(replace-unzipped #;in head #;with replacements)
       . ,(replace-unzipped #;in tail #;with replacements)))

    (_
     (match (and (symbol? expression)
		 (assoc expression replacements))
       ((key . value)
	value)
       (_
	expression)))))

(e.g.
 (replace-unzipped #;in '(a b ... c c ...)
			#;with '((a . a*) (b . b*) (c . c*)))
 ===> (a* b ... c* c ...))

(define (unzip-bindings bindings #;using replacements)
  (let* ((zipped (filter-map (lambda ((name . value))
			       (and-let* (((name . name*) (assoc
							   name
							   replacements)))
				 `(,name* . ,value)))
			     bindings))
	 (((names . values) ...) zipped))
    (map (lambda (value)
	   `(,@(map cons names value) . ,bindings))
	 (transpose values))))

(e.g.
 (unzip-bindings '((a 1 2 3) (b 4 5 6) (c . 3))
		 #;using '((a . a*) (b . b*)))
 ===> (((a* . 1) (b* . 4) (a 1 2 3) (b 4 5 6) (c . 3))
       ((a* . 2) (b* . 5) (a 1 2 3) (b 4 5 6) (c . 3))
       ((a* . 3) (b* . 6) (a 1 2 3) (b 4 5 6) (c . 3))))

(define (unzipped-symbols expression)
  (match expression
    (('quote _)
     '())

    ((zipped (? ...?) . rest)
     (unzipped-symbols (drop rest (prefix-length ...? rest))))
      
    ((head . tail)
     (union (unzipped-symbols head) (unzipped-symbols tail)))
    
    (_
     (if (symbol? expression)
	 `(,expression)
	 '()))))

(e.g. (unzipped-symbols '(a b ... c c ... (d ...) ...)) ===> (c a))

(define (fill template #;with bindings)
  (match template
    
    ((compound (? ...?) . rest)
     (let* ((symbols (unzipped-symbols compound))
	    (unit-symbols (map unique-symbol symbols))
	    (replacements (map cons symbols unit-symbols))
	    (compound (replace-unzipped compound replacements))
	    (unzipped (unzip-bindings bindings #;using replacements))
	    (filled (map (lambda (subbindings)
			   (fill compound #;with subbindings))
			 unzipped))
	    (n (prefix-length ...? rest))
	    (rest (drop rest n)))
       `(,@((iterations n concatenate) filled) . ,(fill rest #;with bindings))))

    
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
 (fill '((A (A ...) A) ...)
       (bind '((A (A ...) A) ...)
	     '((a (a b c d e) a)
	       (b (a b c d e) b)
	       (c (a b c d e) c)
	       (d (a b c d e) d)
	       (e (a b c d e) e))))
 ===> ((a (a b c d e) a)
       (b (a b c d e) b)
       (c (a b c d e) c)
       (d (a b c d e) d)
       (e (a b c d e) e)))
