#!/bin/bash
if [ -n "$STACKSOLVER" ]; then
  export STACKFILE=" --stack-yaml $TRAVIS_BUILD_DIR/integration/StackSolver/$STACKSOLVER "
fi
if [ -n "$LLVM" ]; then
  export LLVMFLAG=" --ghc-options -fllvm --ghc-options -pgmlo --ghc-options opt-$LLVM --ghc-options -pgmlc --ghc-options llc-$LLVM "
fi
if [ -n "$THREADED" ]; then
  export THREADFLAG=" --ghc-options -threaded "
fi
stack install glob-launch $STACKFILE  --ghc-options -O2 $THREADFLAG $LLVMFLAG
