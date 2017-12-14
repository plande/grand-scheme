(define-module (grand scheme)
  #:use-module (grand syntax)
  #:use-module (grand examples)
  ;;#:use-module (grand expand)
  #:use-module (grand publishing)
  #:use-module (grand default-specific)
  #:use-module (grand define-memoized)
  #:use-module (grand define-partial)
  #:use-module (grand define-keywords)
  #:use-module (grand combinatorics)
  #:use-module (grand attributes)
  #:use-module (grand set)
  #:use-module (grand list)
  #:use-module (grand function)
  #:use-module (grand string)
  #:use-module (grand symbol)
  #:use-module (grand numbers)
  #:use-module (grand improper)
  #:use-module (grand binding)
  #:use-module (grand matrix)
  #:use-module (grand guilt)
  #:use-module (grand reading)
  #:use-module (grand cursor)
  #:use-module (grand endian)
  #:use-module (ice-9 pretty-print)
  #:re-export (e.g.
	       pretty-print
	       bind
	       fill
	       argmin
	       argmax
	       min+max
	       argmin+argmax
	       skip
	       alter
	       generate-list
	       with-default
	       without-default
	       specify
	       rest
	       equivalence-classes
	       subsets
	       set-partitions
	       number-partitions
	       number-compositions
	       number->symbol
	       ->string
	       sum
	       product
	       fold-left
	       fold-right
	       unfold-left
	       unfold-left-upto
	       unfold-left-until
	       unfold-right
	       unfold-right-upto
	       unfold-right-until
	       scan
	       prefix-sum
	       extend-left
	       extend-right
	       in?
	       indexed
	       multicombinations
	       cartesian-product
	       cartesian-power
	       map/values
	       map-n
	       prefix?
	       proper-prefix?
	       suffix?
	       proper-suffix?
	       unzip
	       find
	       take-right-while
	       drop-right-while
	       union
	       difference
	       intersection
	       same-sets?
	       subset?
	       set
	       member?
	       powerset
	       insertions
	       prefix-insertions
	       n-insertions
	       permutations
	       take-upto
	       drop-upto
	       memoize
	       define/memoized
	       (define/partial . define+)
	       pass
	       partial
	       maybe
	       either
	       neither
	       both
	       string-matches
	       symbol-match
	       number->symbol
	       symbol-ref
	       symbol-drop
	       every
	       any
	       none
	       filter-map
	       append-map
	       concatenate
	       take
	       take-right
	       take-while
	       drop
	       drop-right
	       drop-while
	       partition
	       split-at
	       span
	       break
	       iota
	       count
	       first last
	       zip
	       first second third fourth fifth
	       delete-duplicates
	       intersperse
	       weave
	       chunks
	       proper-list+dotted-tail
	       publish
	       natural?
	       number/base
	       digits/base
	       let let* lambda define and-let*
	       define-syntax let-syntax letrec-syntax
	       match primitive-lambda
	       is isnt ;; isn't
	       length. any. every.
	       (iterations . times)
	       matrix? M* M+ det inv dim transpose diag
	       zero matrix-ref matrix-column
	       bind-socket
	       select-file-descriptors
	       system-times
	       attribute-ref
	       attributes?
	       attributes+children
	       attributes-children
	       remove-attributes
	       merge-attributes
	       (define/keywords . define*)
	       read-s-expressions
	       read-file
	       read-lines
	       read-line
	       read-delimited
	       ;;expand expand-form
	       cursor?
	       cursor-focus
	       
	       cursor-next
	       cursor-previous
	       cursor-points-to-location?
	       cursor-points-to-expression?
	       cursor-embracing?
	       cursor-parent+location
	       cursor-depth

	       splice-subexpression
	       replace-subexpression
	       remove-subexpression

	       unsigned-little-endian
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
	       64-bits
	       )
  #:replace ((compose/values . compose)
	     ))
