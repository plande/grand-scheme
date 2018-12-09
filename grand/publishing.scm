(define-module (grand publishing)
  #:use-module (grand syntax)
  #:use-module (srfi srfi-1)
  #:use-module (grand expand)
  #:use-module (grand function)
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

(define-macro (publish . stuff)

  (define (definition? x)
    (and-let* ((`(,define-keyword ,interface . ,_) x))
      (is define-keyword member '(define define-syntax define*))))
  
  (define (interface-name interface)
    (match interface
      (`(,head . ,tail)
       (interface-name head))
      (_
       interface)))
  
  (let* ((public `(where . ,private) (break (is _ eq? 'where) stuff))
	 (expanded (map expand-form public))
	 (definitions non-definitions (partition definition? expanded))
	 (names (map (lambda (`(,define-keword ,interface . ,_))
		       (interface-name interface))
		     definitions)))
    `(begin
       ,@(map (lambda (name) `(define ,name #false)) names)
       (let ()
	 ,@private
	 ,@(map (lambda (name definition)
		  `(set! ,name 
			 (let ()
			   ,definition
			   ,name)))
		names definitions)
	 ,@non-definitions))))
