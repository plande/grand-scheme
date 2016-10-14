(define-module (grand improper)
  #:use-module (grand syntax)
  #:use-module (srfi srfi-1)
  #:use-module (grand examples)
  #:export (every. any. length.))

(define (every. pred l)
  (match l
    ((h . t)
     (and (pred h) (every. pred t)))
    (()
     #t)
    (else
     (pred l))))

(define (any. pred l)
  (match l
    ((h . t)
     (or (pred h) (any. pred t)))
    (()
     #f)
    (else
     (pred l))))

(define (length. l)
  (define (inner-length l n)
    (match l
      ((h . t)
       (inner-length t (+ 1 n)))
      (else
       (values n l))))
  (inner-length l 0))

(e.g.
 (length. '(a b c . d)) ===> 3 d)

