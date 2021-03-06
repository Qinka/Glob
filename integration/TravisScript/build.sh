#!/bin/bash

O2FLAG=" --ghc-options -O2 "
if [ -n "$STACKSOLVER" ]; then
  export STACKFILE=" --stack-yaml $TRAVIS_BUILD_DIR/integration/StackSolver/$STACKSOLVER "
fi
if [ -n "$LLVM" ]; then
  export LLVMFLAG=" --ghc-options -fllvm --ghc-options -pgmlo --ghc-options opt-$LLVM --ghc-options -pgmlc --ghc-options llc-$LLVM "
fi
if [ -n "$THREADED" ]; then
  export THREADFLAG=" --ghc-options -threaded "
fi
if [ -n "$DEBUG" ]; then
    export DEBUGFLAG=" --flag glob-core:debug-info "
    if [ -z "$LLVM" ]; then
	O2FLAG=" "
    fi
fi

stack install glob-launch $STACKFILE $O2FLAG $THREADFLAG $LLVMFLAG $DEBUGFLAG
