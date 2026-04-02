# PLCI
**P**ure **L**ambda **C**alculus **I**nterpreter.

## Introducing

This is an interpreter made for anyone who might be interested in experimenting with the *pure lambda calculus*.

Multiple features are still missing, but its got the bare minimum. That is, *Applicative Order Reduction* and *Normal Order Reduction* for evaluation.

Although step-by-step evaluation is not available (*yet*), each strategy has their own unique properties, some of which can be noticed using only evaluation up-to normal form (which is when there aren't redexes any longer).

## Syntax and Semantics

| Syntax | Semantics |
| :----: | :-------- |
| \x.t | An abstraction.<br>If x appears in t, it has<br>to be bound to the<br>most recent abstraction<br>of x |
| t1 t2 | An application.<br>If t1 is an abstraction \x.t11,<br>then (t1 t2) is a redex,<br>and t2 may replace any<br>x within t11 that<br>may be bound to<br>the abstraction<br> |
| x | A variable.<br>It can contain any<br>combination of lowercase<br>characters and end in any<br>number of primes.<br>It can either be bound or<br>free, in which case (the latter)<br>it's name can't change<br>and neither can it be<br>replaced due to a reduction<br>of a redex. |

The λ (lambda) symbol is a synonym of \\.

Applications are conventionally left associative, and abstractions right associative.

Abstracted variables can be curried, which means that ```\x.\y.\z.(x z) (y z)``` is the same as ```\x y z.(x z) (y z)```.

## Running the program

Haskell and Cabal should both be installed.

```cabal build```

```cabal run```

## Reporting issues

Do not forget to report any bugs. Contact me, otherwise you can create a new issue on this repository. Thanks!
