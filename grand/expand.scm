(define-module (grand expand)
  #:use-module (grand syntax)
  #:use-module (srfi srfi-1)
  #:use-module (system base compile)
  #:export (expand expand-form))

(define* (expand-form e #:key (opts '()))
  (let ((exp env (decompile
                  (compile e #:from 'scheme
                    #:to 'tree-il
                    #:env (current-module))
                  #:from 'tree-idl
                  #:to 'scheme
                  #:opts opts)))
    exp))

(define-syntax (expand expression)
  (expand-form 'expression))
