#!/bin/sh

./lhc src/main/ghc/default.ghc > solution/ghost0.ghc
./lhc src/main/ghc/ghost.ghc > solution/ghost1.ghc
./lhc src/main/ghc/pledge.ghc > solution/ghost3.ghc
./lhc src/main/ghc/pledge-reversed.ghc > solution/ghost2.ghc
./llc src/main/lambdalisp/greedy-bot.ll --bot > solution/lambdaman.gcc
