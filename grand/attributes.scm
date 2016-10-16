(define-module (grand attributes)
  #:use-module (grand syntax)
  #:use-module (grand list)
  #:use-module (grand examples)
  #:export (attribute-ref
	    attributes?
	    attributes+children
	    attributes-children
	    remove-attributes
	    merge-attributes))

(define (attribute-ref attributes attribute)
  (and-let* (((key value . rest) attributes))
    (if (eq? attribute key)
	value
	(attribute-ref rest attribute))))

(define (attributes? list)
  (or (null? list)
      (and-let* ((((? keyword?) value . rest) list))
	(attributes? rest))))

(define (attributes+children attribute-list)
  (match attribute-list
    (((? keyword? key) value . rest)
     (let* ((attributes children (attributes+children rest)))
       (values
	`(,key ,value . ,attributes)
	children)))
    (_
     (values '() attribute-list))))

(define (attributes-children attribute-list)
  (match attribute-list
    (((? keyword?) value . rest)
     (attributes-children rest))
    (_
     attribute-list)))

(define (remove-attributes attributes #;from attribute-list)
  (match attribute-list
    (((? keyword? key)  value . rest)
     (if (member key attributes)
	 (remove-attributes attributes rest)
	 `(,key ,value . ,(remove-attributes attributes #;from rest))))
    (_
     attribute-list)))

(define (merge-attributes original #;with delta)
  (fold-left (lambda (original (attribute new-value))
	       (let* ((front rest (break (lambda (x) (eq? x attribute))
					 original)))
		 (match rest
		   ((attribute old-value . rest)
		    `(,@front ,attribute ,new-value ,@rest))
		   (_
		    `(,attribute ,new-value ,@original)))))
	     original
	     (chunks delta 2)))

(e.g.
 (merge-attributes '(#:a 1 #:b 2 #:c 3) #;with '(#:b 4 #:d 4))
 ===> (#:d 4 #:a 1 #:b 4 #:c 3))
