```
// ****************************************************************
// *   Software Failure.  Press left mouse button to continue.    *
// *             Guru Meditation #00000000.00000000               *
// ****************************************************************
```

## What is this?

This is Guru Meditation's entry to the
[ICFP 2014 Contest](http://icfpcontest.org/).

## Who we are

* André Silva ([ShiftForward][shiftforward])
* Hugo Sereno Ferreira ([ShiftForward][shiftforward], [FEUP][feup])
* Joao Azevedo ([ShiftForward][shiftforward])

[feup]: http://www.fe.up.pt
[shiftforward]: http://shiftforward.eu

## Quick-start

Run `sbt assembly` to generate the compiler for the "lambda-man" and the address solver for "ghc". Use the included script `llc` to generate the bot we've submitted (`greedy-bot`) based on the functional programming language LambdaLisp™ (specifically created for ICFPC 2014):

```
./llc src/main/lambdalisp/greedy-bot.ll --bot
```

The `--bot` flag informs the compiler to generate code that loads the maze during startup. The code will be dumped to `stdout`. To generate the ghosts code:

```
./lhc src/main/ghc/ghost-fright.ghc
```

Again, the resulting code will be dumped to `stdout`.

## Solution

We started by implementing a compiler for a simple Lisp, which we called `LambdaLisp`, that targeted the `General Compute Coprocessor`.

After we had the compiler "mostly" working, we were able to start developing a basic standard library for the newly created `LambdaLisp`. The standard library includes combinators for lists (e.g. fold, map, forall, filter), as well as data strucutures (e.g. queues, binary-trees, heaps).

We then implemented the `A*` search algorithm, which was used for path-finding by the `LambdaMan`.

By the end of the contest, we started to implement a ghost AI. We implemented a simple assembler for the `GHost CPU` that allowed us to use labels and it would generate programs with the addressing calculated.
The ghost AI goes in the direction of the `LambdaMan`, and runs away from him if freight mode was enabled. It uses the pledge algorithm for path-finding.
