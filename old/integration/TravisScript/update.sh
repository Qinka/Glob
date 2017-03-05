#!/bin/bash
echo update
############
if [ -n "$IS_DOCKER" ]; then
    export GLOB_VERSION=0.0.10
    export LATEST=glob-$GLOB_VERSION-$(uname)-$OS_DISTRIBUTOR-$OS_CORENAME-GHC_$GHC_VER-$(uname -m)
    export DOCKER_IMAGE_TAG=glob-$GLOB_VERSION-docker
    if [ -n "$TRAVIS_TAG" ]; then
	export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$TRAVIS_TAG
    else
	export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$TRAVIS_BRANCH-${TRAVIS_COMMIT:0:7}
    fi
    export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$(uname)-$OS_DISTRIBUTOR-$OS_CORENAME-GHC_$GHC_VER-$(uname -m)
    if [ -n "$LLVM" ]; then
	export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-llvm-$LLVM
	export LATEST=$LATEST-llvm-$LLVM
    fi
    if [ -n "$THREADED" ]; then
	export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-threaded
	export LATEST=$LATEST-threaded
    fi
    if [ -n "$DEBUG" ]; then
	export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-debug
	export DEBUG_EXT=.debug
    fi
    echo copy files
    cd $TRAVIS_BUILD_DIR
    mkdir docker.tmp
    mkdir docker.tmp/bin
    sudo cp $HOME/.local/bin/glob-launch docker.tmp/bin
    if [ -n "$DEBUG" ]; then
	sudo cp $TRAVIS_BUILD_DIR/integration/Dockerfiles/hub/Dockerfile.debug docker.tmp/Dockerfile
	sudo cp $TRAVIS_BUILD_DIR/integration/ShellScript/entrypoint.py docker.tmp
    else
	sudo cp $TRAVIS_BUILD_DIR/integration/Dockerfiles/hub/Dockerfile docker.tmp
	sudo cp $TRAVIS_BUILD_DIR/integration/ShellScript/entrypoint.sh docker.tmp
    fi
    echo build docker
    cd docker.tmp
    docker build -t qinka/glob:$DOCKER_IMAGE_TAG .
    if [ -z "$DEBUG" ]; then
	docker build -t qinka/glob:$LATEST .
    fi
    docker push  qinka/glob
fi
