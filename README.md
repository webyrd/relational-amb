# relational-amb

A Relational Exploration of McCarthy's amb

Code repository for William E. Byrd's FOSDEM 2022 Declarative and Minimalistic Computing Devroom talk.

Title:
A Relational Exploration of McCarthy's 'Amb'

Abstract:
We will investigate what weirdness occurs when John McCarthy's non-deterministic 'amb' operator is added to a LISP interpreter written in a relational style, in the miniKanren constraint logic programming language.

The code has been tested in Chez Scheme, but should also run in Racket and Guile, with perhaps minor changes to packaging of the files.

The miniKanren implementation is from Michael Ballantyne's 'faster-miniKanren' (https://github.com/michaelballantyne/faster-minikanren).  I made no changes to miniKanren itself; rather, 'amb' is implemented as part of the relational interpreters.

The 'interp-simple-with-amb.scm' interpreter is a slightly modified version of 'interp-simple.scm' from Barliman (https://github.com/webyrd/Barliman/blob/master/cocoa/Barliman/mk-and-rel-interp/interp-simple.scm).

The 'quines-interp-with-amb.scm' interpreter is a slightly modified version of the Quines-generating interpreter from the 2012 Scheme Workshop paper, 'miniKanren, Live and Untagged: Quine Generation via Relational Interpreters (Programming Pearl)' (http://webyrd.net/quines/quines.pdf).

Example usage in Chez Scheme:

```
Chez Scheme Version 9.5.5
Copyright 1984-2020 Cisco Systems, Inc.

> (load "test-amb-quines.scm")
Testing "1 quine"
Testing "2 quines"
Testing "amb-quine-forward-1"
Testing "amb-quine-forward-2"
Testing "amb-quine-forward-3"
Testing "structured-amb-quine-1"
Testing "2 twines"
Testing "3 twines absento"
> 

Process scheme finished
```

```
Chez Scheme Version 9.5.5
Copyright 1984-2020 Cisco Systems, Inc.

> (load "test-interp-simple-with-amb.scm")
Testing "interp-simple-with-amb-1"
Testing "interp-simple-with-amb-2"
Testing "interp-simple-with-amb-3"
Testing "interp-simple-with-amb-4"
Testing "interp-simple-with-amb-5"
Testing "interp-simple-with-amb-6"
Testing "interp-simple-with-amb-7"
Testing "interp-simple-with-amb-8"
Testing "interp-simple-with-amb-9"
Testing "interp-simple-with-amb-10"
Testing "interp-simple-with-amb-11"
Testing "interp-simple-with-amb-12"
Testing "interp-simple-with-amb-13"
Testing "interp-simple-with-amb-14"
Testing "interp-simple-with-amb-15"
Testing "interp-simple-with-amb-16"
Testing "interp-simple-with-amb-17"
Testing "interp-simple-with-amb-18"
Testing "interp-simple-with-amb-19"
Testing "type-inferencer-amb-1"
Testing "type-inferencer-amb-2"
Testing "type-inferencer-3"
Testing "type-inferencer-4"
Testing "interp-simple-with-amb-sicp-1"
Testing "grammar-1"
Testing "grammar-2"
Testing "grammar-3"
Testing "grammar-4"
Testing "grammar-generate-1"
> 
```
