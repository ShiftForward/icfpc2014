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

Run `sbt assembly` to generate the compiler for the "lambda-man" and the address
solver for "ghc". Use the included script `llc` to generate the bot we've
submitted (`greedy-bot`) based on the functional programming language
LambdaLisp™ (specifically created for ICFPC 2014):

```
./llc src/main/lambdalisp/greedy-bot.ll --bot
```

The `--bot` flag informs the compiler to generate code that loads the maze
during startup. The code will be dumped to `stdout`. To generate the ghosts
code:

```
./lhc src/main/ghc/ghost-fright.ghc
```

Again, the resulting code will be dumped to `stdout`.

Here's an example random bot written in `LambdaLisp`:

```scheme
(include prelude.ll)
(include random.ll)

(randomnot: [v m]
   (let ((r (mod (random) m)))
     (tif (= r v)
          (recur v m)
          r)))

(main: [state]
  (let ((map (car state))
        (lambdaman (car (cdr state)))
        (ghosts (car (cdr (cdr state))))
        (fruit (cdr (cdr (cdr state)))))
     (let ((location (car (cdr lambdaman)))
          (direction (car (cdr (cdr lambdaman)))))

        (if (or (or (or (and (= direction 0) (= (nnth map (- (cdr location) 1) (car location)) 0))
                        (and (= direction 1) (= (nnth map (cdr location) (+ (car location) 1)) 0)))
                        (and (= direction 2) (= (nnth map (+ (cdr location) 1) (car location)) 0)))
                        (and (= direction 3) (= (nnth map (cdr location) (- (car location) 1)) 0)))
         (cons 0 (randomnot direction 4))
         (cons 0 direction)))))

(cons 0 main)
```

And the generated `General Compute Coprocessor` assembly can be seen [here](https://gist.github.com/andrebeat/310a96fae981c6af71c0).

## Solution

We started by implementing a compiler for a simple Lisp, which we called
`LambdaLisp`, that targeted the `General Compute Coprocessor`. The
implementation of `LambdaLisp` was pretty much following through the
specification. In order to have a global namespace, we created an initial
environment frame of configurable size filled with zeros. Whenever we were
transpiling `LambdaLisp` to `GCC`, we kept a stack of names to environment
positions for the local namespaces and a single map of names to environment
positions for the global namespace. That way, the position of the environment
for the global namespace was always given by the size of the stack of local
environments. This allowed us to declare some global methods and variables that
were available for the whole `LambdaLisp` program without producing a gigantic
`LET`.

After we had the compiler "mostly" working, we were able to start developing a
basic standard library for the newly created `LambdaLisp`. The standard library
includes combinators for lists (e.g. fold, map, forall, filter), as well as data
strucutures (e.g. queues, binary-trees, heaps).

We then implemented the `A*` search algorithm, which was used for path-finding
by the `LambdaMan`. Our strategy for the `LambdaMan` was as greedy as possible:
try to get to the position that provided the larger amount of points
immediately, while avoiding the ghosts. This was not the smartest of strategies,
since it would be useful to keep power pills uneaten while it wasn't possible to
reach ghosts in time, but still provided decent scores for the Hall of Fame maps
(we were #1 in the ghostbusters map for a while). By the time we got our
strategy going, we started struggling with the limit of instructions per
program. We relied heavily on our standard library but our methods were
sub-optimal regarding the produced instructions size. For example, we initially
started by flattening the input map in order to insert it in a binary tree for
quicker index-based access and this operation by itself was enough to reach the
instruction limit in larger maps. Consumed by a mix of tiredness and frustation,
in a faint glimpse of hope, we decided to use the map size as the deciding
factor of whether we would use the previously described strategy or a completely
random one. This means that in a lot of the judge's maps a random strategy was
in use.

By the end of the contest, we started to implement a ghost AI. We implemented a
simple assembler for the `GHost CPU` that allowed us to use labels and it would
generate programs with the addressing calculated.  The ghost AI goes in the
direction of the `LambdaMan`, and runs away from him if freight mode was
enabled. It uses the pledge algorithm for path-finding.
