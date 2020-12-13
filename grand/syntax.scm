(define-module (grand syntax)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (grand examples)
  #:re-export (match)
  #:export ((and-let*/match . and-let*)
	    with-procedure-properties
	    primitive-lambda
	    is
	    isnt)
  #:replace ((cdefine . define)
	     (smlambda . lambda)
	     (named-match-let-values . let)
	     (or/values . or)
	     (match-let*-values . let*)
	     (letrec-syntax/rules . letrec-syntax)
	     (let-syntax/rules . let-syntax)
	     (define-syntax/rules . define-syntax)))

;; This module extends the syntax of a few core forms so that their
;; "abuses" behave meaningfully -- in particular, it allows to
;; destructure bindings in "lambda" and "let" forms and use curried definitions.
;; It also provides pattern-matching version of "and-let*".

(define-syntax-rule (with-procedure-properties ((property
						 value)
						...)
					       procedure)
  (let ((target procedure))
    (set-procedure-property! target 'property value)
    ...
    target))

(define-syntax-rule (without-procedure-properties _ procedure)
  procedure)

(define-syntax define-syntax/rules
  (syntax-rules ()
    ((_ (name . pattern) template)
     (define-syntax name
       (syntax-rules ()
	 ((name . pattern) template))))
    
    ((_ (name . pattern) . expansion)
     (define-syntax name
       (syntax-rules ()
	 ((name . pattern) (begin . expansion)))))

    ((_ name transformer)
     (define-syntax name transformer))

    ((_ name keywords (pattern template) ...)
     (define-syntax name
       (syntax-rules keywords
	 (pattern
	  template)
	 ...)))

    ((_ name keywords (pattern . expansion) ...)
     (define-syntax name
       (syntax-rules keywords
	 (pattern
	  (begin . expansion))
	 ...)))
    ))

(define-syntax let*-syntax/rules
  (syntax-rules ()
    ((_ let*-syntax () processed-bindings body . *)
     (let*-syntax processed-bindings body . *))
    
    ((_ let*-syntax (((name pattern ...) template)
		     bindings ...) 
	(processed ...) body . *)
     (let*-syntax/rules
      let*-syntax
      (bindings ...) 
      (processed ... (name (syntax-rules () 
			     ((_ pattern ...) 
			      template))))
      body . *))

    ((_ let*-syntax ((name value) bindings ...)
	(processed ...) body . *)
     (let*-syntax/rules
      let*-syntax
      (bindings ...) 
      (processed ... (name value))
      body . *))

    ((_ let*-syntax ((name keywords (pattern template) ...)
		     bindings ...)
	(processed ...)
	body . *)
     (let*-syntax/rules
      let*-syntax
      (bindings ...)
      (processed ... (name (syntax-rules keywords 
			     (pattern 
			      template) 
			     ...)))
      body . *))
    ))

(define-syntax let-syntax/rules
  (syntax-rules ()
    ((_ (bindings ...) body . *)
     (let*-syntax/rules let-syntax (bindings ...) ()
			body . *))))

(define-syntax letrec-syntax/rules
  (syntax-rules ()
    ((_ (bindings ...) body . *)
     (let*-syntax/rules letrec-syntax (bindings ...)
			() body . *))))

(define-syntax mlambda
  (lambda (stx)
    (syntax-case stx ()
      ;; lambda with no body is treated as a pattern predicate,
      ;; returning #t if the argument matches, and #f otherwise
      ((_ args)
       #'(lambda args*
	   (match args*
	     (args #t)
	     (_ #f))))

      ;; in case of patterns consisting entitely
      ;; of identifiers, we desugar to the core lambda
      ((_ (first-arg ... last-arg . rest-args) . body)
       (and (every identifier? #'(first-arg ... last-arg))
	    (or (identifier? #'rest-args) (null? #'rest-args)))
       #'(lambda (first-arg ... last-arg . rest-args) . body))

      ((_ arg body ...)
       (or (identifier? #'arg) (null? #'arg))
       #'(lambda arg body ...))

      ((_ pattern body ...)
       #'(lambda args
	   (match args
	     (pattern body ...) 
	     (_ (error
		 'mlambda
		 '(pattern body ...)
		 args)))))
      )))

(define-syntax-rule (smlambda args . body)
  (without-procedure-properties ((source '(lambda args . body)))
    (mlambda args . body)))

(define-syntax primitive-lambda
  (syntax-rules ()
    ((_ . whatever)
     (lambda . whatever))))

(define-syntax cdefine-bodyless
  (syntax-rules ()
   ((_ (function . pattern) result . args)
    (cdefine-bodyless function
		      (match arg
			(pattern result)
			(_ #f))
		      arg . args))
   
   ((_ name result args ... arg)
    (cdefine-bodyless name (lambda arg result) args ...))

   ((_ name value)
    (define name value))
   ))


(define-syntax cdefine
  (syntax-rules (is)
    ((_ (is x special?) body ...)
     (cdefine-bodyless (special? x) (begin body ...)))

    ((_ (is x related-to? y) body ...)
     (cdefine-bodyless (related-to? x y) (begin body ...)))
    
    ;; for consistency with bodyless lambdas,
    ;; bodyless curried definitions only pattern-match
    ;; on their arguments, and return #t if all matches
    ;; succeed, or #f if some of the matches fail
    ((_ (prototype . args))
     (cdefine-bodyless (prototype . args) #t))

    ((_ ((head . tail) . args) body ...)
     (cdefine (head . tail)
	      (mlambda args body ...)))
    ((_ (function . args) body ...)
     (define function
       (without-procedure-properties ((name 'function))
	 (smlambda args body ...))))
    ((_ . rest)
     (define . rest))
    ))

(define-syntax list<-values
  (syntax-rules ()
    ((_ call)
     (call-with-values (lambda () call) list))))

(define-syntax match-let/error
  (syntax-rules ()
    ((_ ((structure expression) ...)
	body + ...)
     ((lambda args
	(match args
	  ((structure ...) body + ...)
	  (_ (error 'match-let/error
		    (current-source-location) 
		    '((structure expression) ...)
		    expression ...))))
      expression ...))))

(define-syntax named-match-let-values
  (lambda (stx)
    (syntax-case stx (values)
      ((_ ((identifier expression) ...) 
	  body + ...)
       (every identifier? #'(identifier ...))
       ;; optimization: plain "let" form
       #'(let ((identifier expression) ...)
	   body + ...))

      ((_ name ((identifier expression) ...) 
	  body + ...)
       (and (identifier? #'name)
	    (every identifier? #'(identifier ...)))
       ;; optimization: regular named-let
       #'(let name ((identifier expression) ...)
	   body + ...))

      ((_ name (((values . structure) expression))
	  body + ...)
       (identifier? #'name)
       #'(letrec ((name (mlambda structure body + ...)))
	   (call-with-values (lambda () expression) name)))

      ((_ (((values . structure) expression)) body + ...)
       #'(call-with-values (lambda () expression)
	   (mlambda structure body + ...)))

      ((_ name ((structure expression) ...)
	  body + ...)
       (and (identifier? #'name)
	    (any (lambda (pattern)
		   (display pattern)(newline)
		   (and (pair? pattern)
			(eq? (car pattern) 'values)))
		 (syntax->datum #'(structure ...))))
       #'(syntax-error "let can only handle one binding \
in the presence of a multiple-value binding"))
      
      ((_ name ((structure expression) ...)
	  body + ...)
       (identifier? #'name)
       #'(letrec ((name (mlambda (structure ...) body + ...)))
	   (name expression ...)))

      ((_ ((structure expression) ...)
	  body + ...)
       (any (lambda (pattern)
	      (and (pair? pattern)
		   (eq? (car pattern) 'values)))
	    (syntax->datum #'(structure ...)))
       #'(syntax-error "let can only handle one binding \
in the presence of a multiple-value binding"))
      
      ((_ ((structure expression) ...)
	  body + ...)
       #'(match-let/error ((structure expression) ...) 
			  body + ...))

      ((_ ((structure structures ... expression)) body + ...)
       #'(call-with-values (lambda () expression)
	   (mlambda (structure structures ... . _)
		    body + ...)))

      ((_ name ((structure structures ... expression))
		body + ...)
       (identifier? #'name)
       #'(letrec ((name (mlambda (structure structures ...)
			     body + ...)))
	   (call-with-values (lambda () expression) name)))

      ;; it should generally be discouraged to use the plain
      ;; let with multiple values, because there's no natural
      ;; way to implement that when there's more than one
      ;; (multiple-value) binding,
      ;; but it could be added for completeness
#|
      ((_ ((structure structures ... expression) ...)
	  body + ...)
       #'(match-let/error (((structure structures ... . _) 
			    (list<-values expression)) ...)
			  body + ...))
      
      ((_ name ((structure structures ... expression) ...)
	  body + ...)
       (identifier? #'name)
       #'(letrec ((loop
		   (lambda args
		     (match args
		       (((structure structures ... . _) ...)
			(let-syntax ((name
				      (syntax-rules ()
					((_ args (... ...))
					 (loop (list<-values
						args)
					       (... ...))))))
			  body + ...))
		     (_ (error 'named-match-let-values 
			       (current-source-location)
			       'name)))))
		  (loop (list<-values expression) ...)))
|#
      )))

(define-syntax or/values
  (syntax-rules ()
    ((_)
     #false)
    
    ((or/values final)
     final)
    
    ((or/values first . rest)
     (call-with-values (lambda () first)
       (lambda result
	 (if (and (pair? result) (car result))
	     (apply values result)
	     (or/values . rest)))))))

(define-syntax match-let*-values
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((identifier expression) ...) body + ...)
       (every identifier? #'(identifier ...))
       ;; optimization/base case: regular let*
       #'(let* ((identifier expression) ...)
	   body + ...))
      ((_ (binding . bindings) body + ...)
       #'(named-match-let-values (binding)
				 (match-let*-values
				  bindings
				  body + ...))))))

(define-syntax and-let*/match
  (lambda (stx)
    (syntax-case stx (values)

      ((_)
       #'#t)

      ((_ ())
       #'#t)

      ((_ () body ...)
       #'(let () body ...))

      ((_ ((name binding) rest ...) body ...)
       (identifier? #'name)
       #'(let ((name binding))
	   (and name
		(and-let*/match (rest ...)
				body ...))))

      ((_ (((values . structure) binding) rest ...) body ...)
       #'(call-with-values (lambda () expression)
	   (lambda args
	     (match args
	       (structure
		(and-let*/match (rest ...)
				body ...))
	       (_ #f)))))
      
      ((_ ((value binding) rest ...) body ...)
       #'(match binding
	   (value
	    (and-let*/match (rest ...)
	      body ...))
	   (_ #f)))

      ((_ ((condition) rest ...)
	  body ...)
       #'(and condition
	      (and-let*/match (rest ...)
		body ...)))

      ((_ ((value * ... expression) rest ...) body ...)
       (identifier? #'value)
       #'(call-with-values (lambda () expression)
	   (lambda args
	     (match args
	       ((value * ... . _)
		(and value
		     (and-let*/match (rest ...)
				     body ...)))
	       (_ #f)))))

      ((_ ((value ... expression) rest ...) body ...)
       #'(call-with-values (lambda () expression)
	   (lambda args
	     (match args
	       ((value ... . _)
		(and-let*/match (rest ...)
				body ...))
	       (_ #f)))))

      )))

(define-syntax infix/postfix
  (syntax-rules ()
    
    ((infix/postfix x somewhat?)
     (somewhat? x))

    ((infix/postfix left related-to? right)
     (related-to? left right))

    ((infix/postfix left related-to? right . likewise)
     (let ((right* right))
       (and (infix/postfix left related-to? right*)
	    (infix/postfix right* . likewise))))))

(define-syntax extract-_
  (syntax-rules (_ is isnt quote
 		   quasiquote unquote
		   unquote-splicing)
    ;; ok, it's a bit rough, so it requires an explanation.
    ;; the macro operates on sequences of triples
    ;;
    ;;   (<remaining-expr> <arg-list> <processed-expr>) +
    ;;
    ;; where <remaining-expr> is being systematically
    ;; rewritten to <processed-expr>. When the _ symbol
    ;; is encountered, it is replaced with a fresh "arg"
    ;; symbol, which is appended to both <arg-list>
    ;; and <processed-expr>.
    ;;
    ;; The goal is to create a lambda where each
    ;; consecutive _ is treated as a new argument
    ;; -- unless there are no _s: then we do not
    ;; create a lambda, but a plain expression.
    ;;
    ;; The nested "is" and "isnt" operators are treated
    ;; specially, in that the _s within those operators are
    ;; not extracted.
    ;;
    ;; Similarly, the _ isn't extracted from quoted forms,
    ;; and is only extracted from quasi-quoted forms if
    ;; it appears on unquoted positions.

    ;; The support for quasiquote modifies the tuples
    ;; to have the form
    ;;
    ;;   (<remaining-expr> <arg-list> <processed-expr> . qq*) +
    ;;
    ;; where qq* is a sequence of objects that expresses
    ;; the nesting level of the 'quasiquote' operator
    ;; (i.e. quasiquote inside quasiquote etc.)

    ;; The macro consists of the following cases:
    
    ;; fin case with no _s
    ((extract-_ fin (() () body))
     (fin (infix/postfix . body)))

    ;; fin case with some _s -- generate a lambda
    ((extract-_ fin (() args body))
     (lambda args (fin (infix/postfix . body))))

    ;; treat 'is' and 'isnt' operators specially and
    ;; don't touch their _s
    ((extract-_ fin (((is . t) . rest) args (body ...)) . *)
     (extract-_ fin (rest args (body ... (is . t))) . *))

    ((extract-_ fin (((isnt . t) . rest) args (body ...)) . *)
     (extract-_ fin (rest args (body ... (isnt . t))) . *))

    ;; same with 'quote'
    ((extract-_ fin (('literal . rest) args (body ...)) . *)
     (extract-_ fin (rest args (body ... 'literal)) . *))

    ;; when 'quasiquote' is encountered, we increase the
    ;; level of quasiquotation (the length of the qq* sequence)
    ((extract-_ fin
		(((quasiquote x) . rest) args body . qq*) . *)
     (extract-_ fin
		((x) () (quasiquote) qq . qq*)
		(rest args body) . *))

    ;; on the other hand, for 'unquote' and
    ;; 'unquote-splicing', we decrease the nesting level
    ;; (i.e. we consume one element from the qq* sequence)
    ((extract-_ fin
		(((unquote x) . rest) args body qq . qq*) . *)
     (extract-_ fin
		((x) () (unquote) . qq*)
		(rest args body qq . qq*) . *))

    ((extract-_ fin
		(((unquote-splicing x) . rest) args body
		 qq . qq*) . *)
     (extract-_ fin
		((x) () (unquote-splicing) . qq*)
		(rest args body qq . qq*) . *))

    ;; push/unnest nested expression for processing
    ((extract-_ fin (((h . t) . rest) args body . qq) . *)
     (extract-_ fin ((h . t) () () . qq)
		(rest args body . qq) . *))

    ;; unquote in the tail position
    ((extract-_ fin
		((unquote x) args (body ...) qq . qq*) . *)
     (extract-_ fin
		((x) args (body ... unquote) . qq*) . *))
    
    ;; generate a new arg for the _ in the head position
    ((extract-_ fin ((_ . rest) (args ...) (body ...)) . *)
     (extract-_ fin (rest (args ... arg) (body ... arg)) . *))

    ;; rewrite the term in the head position to the back
    ;; of the processed terms
    ((extract-_ fin ((term . rest) args (body ...) . qq) . *)
     (extract-_ fin (rest args (body ... term) . qq) . *))

    ;; _ in the tail position
    ((extract-_ fin
		(_ (args ...) (body ...) . qq)
		(rest (args+ ...) (body+ ...) . qq+) . *)
     (extract-_ fin
		(rest (args+ ... args ... arg)
		      (body+ ... (body ... . arg)) . qq+) . *))

    ;; pop/nest back processed expression
    ;; ('last' is an atom; most likely (), but can also
    ;; be some value, e.g. in the case of assoc list literals)
    ((extract-_ fin
		(last (args ...) (body ...) . qq)
		(rest (args+ ...) (body+ ...) . qq+) . *)
     (extract-_ fin (rest (args+ ... args ...)
			  (body+ ... (body ... . last))
			  . qq+) . *))
    ))


(define-syntax-rule (identity-syntax form)
  form)

(define-syntax-rule (is . something)
  (extract-_ identity-syntax (something () ())))

(define-syntax-rule (isnt . something)
  (extract-_ not (something () ())))

(e.g.
 (is 2 < 3))

(e.g.
 (is 1 < 2 = (+ 1 1) <= 3 odd?))

(e.g.
 (filter (is 5 < _ <= 10) '(1 3 5 7 9 11))
 ===> (7 9))

(e.g.
 (let ((<* (is (length _) < (length _))))
   (is '(1 2 3) <* '(1 2 3 4))))

(e.g.
 ((is (length (filter (is (modulo _ 2) = 0) _)) < 5)
  '(1 2 3 4 5 6 7 8 9)))

(e.g.
 (is 2 even?))

(e.g.
 (isnt 2 odd?))

(e.g.
 (every (is (+ _ _) even?) '(1 2 3 4) '(5 6 7 8)))

(e.g.
 ((is 2 _ 3) <))

(e.g.
 ((isnt 2 _ 3) >=))

(e.g.
 ((is _ _ _) 2 < 3))

(e.g.
 ((is (expt _ 2) _ (expt _ 2) _ (expt _ 2)) 2 <= -2 < -3))

(e.g. ;; a bit contrived, but shows handling of quotations
 ((is '(_ _) list `(_ ,_ ,@(_ _) `,,_ ,'_)) 5 values '(6 7) 'X)
 ===> ((_ _) (_ 5 6 7 `,X _)))

(e.g. ;; handling improper lists
 (is '(x . y) member '((a . b) (p . q) (x . y))))

(e.g.
 ((is `(p . ,_) member `((a . b) (,_ . q) (x . y)))
  'q 'p))
