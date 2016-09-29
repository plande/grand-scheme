(define-module (grand string)
  #:use-module (grand syntax)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (grand examples)
  #:export (->string string-matches))

(define (->string object)
  (cond ((symbol? object)
	 (symbol->string object))
	((number? object)
	 (number->string object))
	((list? object)
	 (string-append
	  "(" (string-join (map ->string object) " ") ")"))
	((pair? object)
	 (string-append
	  "(" (->string (car object)) " . " (->string (cdr object))))
	((string? object)
	 object)
	((vector? object)
	 (string-append "#" (->string (vector->list object))))
	(else
	 (with-output-to-string
	   (lambda ()
	     (display object))))))

(define (string-matches pattern string)
  (let ((regex (make-regexp pattern)))
    (let loop ((n 0)
	       (all '()))
      (let ((m (regexp-exec regex string n)))
	(if m
	    (let ((c (match:count m)))
	      (loop (match:end m)
		    (cons (if (= c 1)
			      (match:substring m)
			      (map (lambda (n) (match:substring m n))
				   (iota (- c 1) 1)))
			  all)))
	    (reverse all))))))

;; if pattern contains no parentheses, string-matches returns a list of
;; all substrings that match a given pattern

(e.g.
 (string-matches "[0-9]" "1a 2b 3c 4d")
 ===> ("1" "2" "3" "4"))

;; if parenthesized expressions appear within the pattern,
;; it returns a list of lists of strings, corresponding to
;; subsequent groups

(e.g.
 (string-matches "([0-9])([a-z])" "1a 2b 3c 4d")
 ===> (("1" "a") ("2" "b") ("3" "c") ("4" "d")))
