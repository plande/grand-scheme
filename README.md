# The Grand Scheme Glossary

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
his own set of functions, or "a personal library",
in order to be able to do useful things.

This situation has both its pros and cons.
The pros are that Scheme programmers usually
know their language very well.
The cons are that it may be difficult for two
Scheme programmers to agree on how to express certain
ideas.

One solution to this problem was proposed in the
late 1990s., when the Scheme Request for Implementation
process was implemented.

Unlike the cultures of many successful programming
languages, which celebrate the figure of the so-called
Benevolent Dictator For Life -- a single person that is
responsible for incorporating new ideas into the core
language, the Scheme culture has always been focused
on discussion. However, it seems that the whole SRFI
process quickly became bureaucratic.

## SRFI-1 controversies

Even the now widely available SRFI-1 library
contains some controversial definitions.

For example, it provides a definition of fold,
which is essentially broken. "Fold" is a general idea,
that given a list of elements, say, `[a, b, c, d]`,
and an operator, say `*`, one gets a value of `a*b*c*d`.

Unless the operator `*` is associative, this formulation
is imprecise. We can interpret the expression `a*b*c*d`
either as `(((a*b)*c)*d)`, or as `(a*(b*(c*d)))`, or
as `((a*b)*(c*d))`. The first variant is called `fold-left`,
the second `fold-right`, and the third isn't provided
in SRFI-1, but could be called `fold-balanced`.

