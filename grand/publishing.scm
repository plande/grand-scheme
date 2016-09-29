(define-module (grand publishing)
  #:use-module (grand syntax)
  #:use-module (srfi srfi-1)
  #:export-syntax (publish))

;; The `publish' macro is used to provide means to separate public
;; definitions from private ones (such that are visible only from within
;; the public procedures and from within themselves).
;; For example, the form
;; 
;; (publish
;;   (define (f x) (+ a x))
;;   (define (g y) (* a y))
;;  where
;;   (define a 5))
;;
;; is equivalent to
;;
;; (begin
;;   (define f (and (defined? 'f) f))
;;   (define g (and (defined? 'g) g))
;;   (let ()
;;     (define a 5)
;;     (set! f (let () (define (f x) (+ a x)) f))
;;     (set! g (let () (define (g x) (* a y)) g))))

(define-syntax (publish definitions ...)
  (publisher (definitions ...) ()))

(define-syntax publisher (where)
  ((_ (where private ...) (public ...))
   (private+public (private ...) (public ...)))
  ((_ (new defs ...) (approved ...))
   (publisher (defs ...) 
	      (approved ... new))))

(define-syntax private+public
  (lambda (stx)
    (define (sorted-private/interfaces+names+bodies private specs)
      ;; both sorting and name extraction takes place in the
      ;; same function called from with-syntax, because that
      ;; way we can tell the macro processor that the bindings in
      ;; the code belong to the same scope
      (define (interface-name interface)
	(match interface
	  ((head . tail)
	   (interface-name head))
	  ((? symbol? name)
	   name)))
      `(,(datum->syntax ;; this reordering is done, so that the
	  stx ;; (e.g. ...) forms can be freely mixed with definitions
	  (let ((definitions non-definitions
		  (partition (lambda (prototype)
			       (match prototype
				 (((? symbol? x) . _)
				  (string-contains (symbol->string x)
						   "def"))
				 (_ #f)))
			     (syntax->datum private))))
	    `(,@definitions ,@non-definitions)))
	,(map (lambda (spec)
		(syntax-case spec ()
		  ((interface . body)
		   (datum->syntax stx `(,(syntax->datum #'interface)
					,(interface-name 
					  (syntax->datum #'interface))
					,(syntax->datum #'body))))))
	      specs)))
    (syntax-case stx ()
      ((_ (private ...) ((define-variant . spec) ...))
       (with-syntax ((((private ...) ((interface name body) ...))
		      (sorted-private/interfaces+names+bodies 
		       #'(private ...) #'(spec ...))))
	 #'(begin
	     (define name (and (defined? 'name) name))
	     ...
	     (let ()
	       private ...
	       (set! name
		     (let ()
		       (define-variant interface . body)
		       name))
	       ...)))))))
