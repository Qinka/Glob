#!/bin/bash

if [ -n "$RUN_TEST" ]; then
    # for testing
    echo testing
    # display the version of ghc
    ghc -V
    stack test $STACKFILE
fi
