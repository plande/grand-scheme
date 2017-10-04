(define-module (grand endian)
  #:use-module (grand syntax)
  #:use-module (grand list)
  #:use-module (grand numbers)
  #:export (unsigned-little-endian
	    unsigned-little-endian-32
	    unsigned-little-endian-64
	    little-endian
	    little-endian-16
	    little-endian-32
	    little-endian-64
	    unsigned-big-endian
	    unsigned-big-endian-16
	    unsigned-big-endian-32
	    unsigned-big-endian-64
	    big-endian
	    big-endian-16
	    big-endian-32
	    big-endian-64
	    8-bits
	    16-bits
	    32-bits
	    64-bits))

(define (unsigned-little-endian bytes value)
  (let ((cells (reverse ((digits/base 256) value))))
    (extend-right cells #;to bytes #;with 0)))

(define (unsigned-little-endian-16 value)
  (unsigned-little-endian 2 value))

(define (unsigned-little-endian-32 value)
  (unsigned-little-endian 4 value))

(define (unsigned-little-endian-64 value)
  (unsigned-little-endian 8 value))

(define (little-endian bytes value)
  (if (negative? value)
      (unsigned-little-endian bytes (+ value (expt 2 (* 8 bytes))))
      (unsigned-little-endian bytes value)))

(define (little-endian-16 value)
  (little-endian 2 value))

(define (little-endian-32 value)
  (little-endian 4 value))

(define (little-endian-64 value)
  (little-endian 8 value))

(define (unsigned-big-endian bytes value)
  (extend-left ((digits/base 256) value) #;to bytes #;with 0))

(define (unsigned-big-endian-16 value)
  (unsigned-big-endian 2 value))

(define (unsigned-big-endian-32 value)
  (unsigned-big-endian 4 value))

(define (unsigned-big-endian-64 value)
  (unsigned-big-endian 8 value))

(define (big-endian bytes value)
  (if (negative? value)
      (unsigned-big-endian bytes (+ value (expt 2 (* 8 bytes))))
      (unsigned-big-endian bytes value)))

(define (big-endian-16 value)
  (big-endian 2 value))

(define (big-endian-32 value)
  (big-endian 4 value))

(define (big-endian-64 value)
  (big-endian 8 value))

(define (8-bits number)
  (extend-left ((digits/base 2) number) #;to 8 #;with 0))

(define (16-bits number)
  (append-map 8-bits (big-endian 2 number)))

(define (32-bits number)
  (append-map 8-bits (big-endian 4 number)))

(define (64-bits number)
  (append-map 8-bits (big-endian 8 number)))


(e.g. (little-endian-32 (+ 256 119)) ===> (119 1 0 0))
(e.g. (big-endian-32 (+ 256 119)) ===> (0 0 1 119))

(e.g. (8-bits 119) ===> (0 1 1 1 0 1 1 1))
(e.g. (16-bits (* 119 256)) ===> (0 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0))

(e.g. (map 8-bits (little-endian 2 311))
      ===> ((0 0 1 1 0 1 1 1) (0 0 0 0 0 0 0 1)))

(e.g. (map 8-bits (little-endian 2 -312))
      ===> ((1 1 0 0 1 0 0 0) (1 1 1 1 1 1 1 0)))

(e.g. (map 8-bits (unsigned-little-endian 2 -312))
      ===> ((1 1 0 0 1 0 0 0) (1 1 1 1 1 1 1 1)))

(e.g. (map 8-bits (big-endian 2 311))
      ===> ((0 0 0 0 0 0 0 1) (0 0 1 1 0 1 1 1)))

(e.g. (map 8-bits (big-endian 2 -312))
      ===> ((1 1 1 1 1 1 1 0) (1 1 0 0 1 0 0 0)))

(e.g. (map 8-bits (unsigned-big-endian 2 -312))
      ===> ((1 1 1 1 1 1 1 1) (1 1 0 0 1 0 0 0)))
