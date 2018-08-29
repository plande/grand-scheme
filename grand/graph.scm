(define-module (grand graph)
  #:use-module (grand syntax)
  #:use-module (grand function)
  #:use-module (grand list)
  #:use-module (grand set)
  #:use-module (grand examples)
  #:export (reach optimal-path))

(define (reach #;of graph #;from node)
  (define (walk #;from novel #;into visited)
    (let* ((vicinity (fold-left union '() (map graph novel)))
	   (visited (union visited novel))
	   (novel (difference vicinity visited)))
      (if (null? novel)
	  visited
	  (walk #;from novel #;into visited))))
  (walk #;from (graph node) #;into '()))

(e.g.
 (let ((graph '((a b c)
		(b c)
		(c d e)
		(d)
		(e))))
   (define ((callable alist) key)
     (assoc-ref alist key))

   (is (reach #;of (callable graph) #;from 'a)
       same-sets? '(b c d e))))

(define (optimial-path #;on weighted-graph #;from initial-state 
			    #;until success?
				    #;guided-by remaining-cost-estimate)
  (define (probably-shorter? `(,estimate-a . ,_)
			     `(,estimate-b . ,_))
    (is estimate-a < estimate-b))

  (define (walk paths visited-nodes)
    (and-let* ((`((,_ ,cost-so-far ,path) . ,paths) paths)
	       (`(,current-node . ,_) path))
      (define (estimate-total-cost `(,node ,weight))
	(let* ((total-cost (+ cost-so-far weight))
	       (estimate (+ total-cost
			    (remaining-cost-estimate node))))
	  `(,estimate ,total-cost ,node)))

      (define (update-paths paths `(,estimate
				    ,alternative-cost
				    ,node))
	(let ((new-path `(,estimate
			  ,alternative-cost
			  (,node . ,path))))

	  (match (find (lambda (`(,_ ,_ (,end . ,_)))
			 (equal? end node))
		       paths)
	    (`(,previous-estimate ,established-cost ,_)
	     (if (is established-cost <= alternative-cost)
		 paths
		 (let ((paths (only (lambda (`(,_ ,_ (,end . ,_)))
				      (isnt end equal? node))
				    paths)))
		   (merge `(,new-path) paths probably-shorter?))))
	    (_
	     (merge `(,new-path) paths probably-shorter?)))))

      (if (success? current-node)
	  (values (reverse path) cost-so-far)
	  (let* ((neighbors (weighted-graph current-node))
		 (new-neighbors (only (lambda (`(,node ,weight))
					(isnt node member visited-nodes))
				      neighbors))
		 (judged-neighbors (map estimate-total-cost new-neighbors))
		 (paths (fold-left update-paths paths judged-neighbors)))
	    (walk paths (union `(,current-node)
			       visited-nodes))))))

  (walk `((+inf.0 0 (,initial-state))) '()))

(e.g.
 ;; cf. https://upload.wikimedia.org/wikipedia/commons/9/98/AstarExampleEn.gif
 (let ((graph-from-Wikipedia '((start (a 1.5) (d 2))
			       (a (b 2) (start 1.5))
			       (b (c 3) (a 2))
			       (c (end 4) (b 3))
			       (d (e 3) (start 2))
			       (e (end 2) (d 3))
			       (end (c 4) (e 2))))
       (heuristics '((a . 4)
		     (b . 2)
		     (c . 4)
		     (d . 4.5)
		     (e . 2)
		     (end . 0))))
   
   (define ((callable alist) key)
     (assoc-ref alist key))

   (optimial-path #;on (callable graph-from-Wikipedia)
		       #;from 'start
			      #;until (is _ equal? 'end)
				      #;guided-by (callable heuristics)))
  ===> (start d e end) 7)
