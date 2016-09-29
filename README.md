# The Grand Scheme (glossary)

We usually think of software libraries as pieces
of software that does certain things for us.

"Library" is a terrible name for that. Perhaps
"service provider" is a better name. A library
is a place where you can go to read a book,
as it has a lot of them.

This repository contains a set of definitions
of functions that I felt were lacking in the
core language Scheme. A set of definitions
of terms from a particular domain of knowledge
is called a *glossary*, not a library.

Scheme is a rather small programming language.
One could also say that -- unlike most (if not
all) languages -- it is uncluttered. As a result,
every Scheme programmer usually has to write
one's own set of functions, or "a personal library",
in order to be able to do useful things.

This situation has both its pros and cons.
The pros are that Scheme programmers usually
know their language very well.
The cons are that it may be difficult for two
Scheme programmers to agree on how to express certain
ideas.

One solution to this problem was proposed in the
late 1990s., when the [Scheme Request for Implementation](http://srfi.schemers.org)
process was implemented.

Unlike the cultures of many successful programming
languages, which celebrate the figure of the so-called
Benevolent Dictator For Life -- a single person that is
responsible for incorporating new ideas into the core
language, the Scheme culture has always been focused
on discussion. However, it seems that the whole SRFI
process quickly became bureaucratic.

## SRFI-1 controversies

Even the now widely available [SRFI-1](http://srfi.schemers.org/srfi-1/)
library contains some controversial definitions.

### Controversies around `fold`

For example, it provides a definition of fold,
which is essentially broken. "Fold" is a general idea,
that given a list of elements, say, `[a, b, c, d]`,
and an operator, say `*`, one gets a value of `a*b*c*d`.

Unless the operator `*` is associative, this formulation
is imprecise. We can interpret the expression `a*b*c*d`
either as `(((a*b)*c)*d)`, or as `(a*(b*(c*d)))`, or
as `((a*b)*(c*d))`. The first variant is called `fold-left`,
the second `fold-right`, and the third could be called
`fold-balanced`.

Now the authors of SRFI-1 decided to use the name `fold`
to refer to the left `fold`, clearly favoring it over
the other variant. In addition, they reversed the order
of arguments, so that it actually computes `(d*(c*(b*a)))`.
This decision was made despite some people [arguing](http://srfi.schemers.org/srfi-1/mail-archive/msg00021.html)
against it, but no reason for that choice was given.

### Problems with `unfold`

In addition to the left and right variants of `fold`,
SRFI-1 also provides the duals to these, namely -- `unfold`.
Given a unary function `f` and a seed value `x`, the left
variant of `unfold` produces the list `[x, f(x), f(f(x)), f(f(f(x))), ...]`.

The signature of the left variant of `unfold` presented in
the SRFI-1 document is following:
```
unfold p f g seed [tail-gen] -> list
```
So in addition to the `f` and `x` arguments from the above
description (that called `g` and `seed`, respectively), it
takes three other arguments: `p`, `f` and optionally `tail-gen`.
What are those?

The `p` argument is a predicate which tells when `fold`
should terminate. Without it, we would need to apply
certain conventions, so that `g` could tell us when to stop,
because dealing with infinite sequences in eager languages
is a bit problematic.

The role of `f` is best explained by the SRFI-1 document itself:
"the seed values are mapped to list elements by f, producing
the elements of the result list in a left-to-right order".

A-ha! So we know, that the `unfold` function provided by SRFI-1
shouldn't be called `unfold`, but rather `unfold-map`!

The mysterious optional `tail-gen` argument "creates the tail of the list;
defaults to (lambda(x)'())". Wat?

## Solution

The list of problems with the SRFI-1 that I just presented is of course
by no means complete. They mainly serve as an illustration that the SRFI
process is fallible and sometimes delivers questionable solutions.

Languages derived from Lisp have the magical property that they can
be used not necessarily in the way their creators imagined, but instead
they can be used in the way their users like. The advantages of Scheme
are that it assumes very little, and is widely available, even on
systems you've never heard of.

The Grand Scheme library contains a set of extensions that allow
to use Scheme in the way I like -- and I hope you'll like it too.
But rather than relying solely on my personal taste, I try to conform
to a set of rules that make working with the language easier.

The rules are:
- functions should be pure
- language constructs that cause side-effects should be encapsulated
with macros that limit the extent of those side-effects
- if possible, use the `quasiquote` macro, rather than `cons`,
`append` or `list`
- prefer the use of [`match`](http://synthcode.com/scheme/match.scm)
to `cond`, if the patterns are simple
- never use abbreviations
- use commented prepositions to separate arguments, for example:
```
(define (unfold-left-until stop? #;using f #;starting-with seed)
  (if (stop? seed)
    '()
    `(,seed . ,(unfold-left-until stop? #;using f 
                                  #;starting-with (f seed)))))
```
In this way, I find it easier to memoize the order (or role) of arguments
to functions.

The library is intended primarily for Guile Scheme, because this
is the implementation that I've been using most often. For that
reason, I also use the module system of Guile, rather than the
R6RS module system. I hope to change this one day to make the
library more portable, but for now I have some more urgent
things to do.

## The `(grand)` meta-module

The `(grand)` module is a meta-module that re-exports
all the bindings that are contained in the `grand` directory.
Some interesting ones are:
```
(grand examples)
(grand syntax)
(grand publishing)
(grand list)
(grand define-partial)
```

## The `(grand examples)` module

The `(grand examples)` module defines a special `e.g.` form
that is used in other modules or programs to embed examples.
For example, given the above definition of `unfold-left-until`,
one could write an example usage for the purpose of clarification:
```
(e.g.
  (unfold-left-until (lambda (x) (> x 10)) 1+ 0)
  ===> (0 1 2 3 4 5 6 7 8 9 10))
```
If the value of expression is not `equal?` to the
value on the right of the `===>` symbol, an error
is raised.

The `e.g.` form supports multiple values:
```
(e.g. (values 1 2 3) ===> 1 2 3)
```

If a single expression is provided to the `e.g.` form, without
the `===>` symbol nor purported value(s), an error is rased
if the expression evaluates to `#false`
```
(e.g. (and (even? 2) (odd? 2))) ;; raises an error
```

## The `(grand syntax)` module

The `(grand syntax)` module, formerly [known](https://lists.gnu.org/archive/html/guile-user/2015-09/msg00009.html)
as `(ice-9 nice-9)`, extends some of the core bindings of Scheme, in the following way:
- it allows to perform destructured binding with the `let`, `let*`
and `lambda`,
```
(e.g.
  (map (lambda ((a . b)) (+ a b)) '((1 . 2) (3 . 4)(5 . 6)))
  ===> (3 7 11))
```
- it allows the `let*` form to capture multiple values
```
(e.g.
  (let* ((a b (values 1 2)))
    (+ a b)) ===> 3)
```
- it allows for curried definitions, for example
```
(define ((f x) y)
  (+ x y))
```
expands to
```
(define f
  (lambda (x)
    (lambda (y)
      (+ x y))))
```
- it provides a pattern-matching version of the `and-let*`
form, which can also capture multiple values. If a
pattern fails to match, `and-let*` evaluates to `#false`.
- allows to omit `syntax-rules` when using `define-syntax`,
`let-syntax` and `letrec-syntax` in particular ways
- re-exports the `match` syntax from `(ice-9 match)` library,
so that the latter doesn't need to be imported.

