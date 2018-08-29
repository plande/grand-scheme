(define-module (grand symbol)
  #:use-module (grand syntax)
  #:use-module (srfi srfi-1)
  #:use-module (grand string)
  #:use-module (grand examples)
  #:export (symbol-match number->symbol symbol-ref symbol-drop))

(define (symbol-match pattern symbol)
  (and-let* ((string (symbol->string symbol))
	     ((first-match . _) (string-matches pattern string)))
    (map string->symbol first-match)))

(define (number->symbol n)
  (string->symbol (number->string n)))

(define (symbol-ref s n)
  (string-ref (symbol->string s) n))

(define (symbol-drop s n)
  (string->symbol (string-drop (symbol->string s) n)))
