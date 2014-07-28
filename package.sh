#!/bin/sh
./lhc src/main/ghc/pledge.ghc > solution/ghost0.ghc
./lhc src/main/ghc/pledge-reversed.ghc > solution/ghost1.ghc
./llc src/main/lambdalisp/greedy-bot.ll --bot > solution/lambdaman.gcc
