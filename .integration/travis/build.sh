#!/bin/bash
ghc -V

if [ -n "$STACKSOLVER" ]; then
    export COMMON_OPTIONS=$COMMON_OPTIONS" --stack-yaml $TRAVIS_BUILD_DIR/.stack-yaml/$STACKSOLVER "
fi
if [ -n "$LLVM" ]; then
    export COMMON_OPTIONS=$COMMON_OPTIONS" --ghc-options -fllvm --ghc-options -pgmlo --ghc-options opt-$LLVM --ghc-options -pgmlc --ghc-options llc-$LLVM "
fi
if [ -n "$THREADED" ]; then
    export COMMON_OPTIONS=$COMMON_OPTIONS" --ghc-options -threaded "
fi
if [ -n "$DEBUG" ]; then
    export BUILD_OPTIONS=$BUILD_OPTIONS" --ghc-options -rtsopts "
else
    export COMMON_OPTIONS=$COMMON_OPTIONS" --ghc-options -O3 "
fi

stack test $COMMON_OPTIONS
stack install $COMMON_OPTIONS $BUILD_OPTIONS
