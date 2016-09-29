(define-module (grand numbers)
  #:export (natural?))

(define (natural? x)
  (and (integer? x)
       (>= x 0)))
