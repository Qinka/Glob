#!/bin/bash
echo update
############
export GLOB_VERSION=0.0.10
export LATEST=glob-$GLOB_VERSION-$(uname)-$OS_DISTRIBUTOR-$OS_CORENAME-GHC_$GHC_VER-$(uname -m)
export DOCKER_IMAGE_TAG=glob-$GLOB_VERSION-docker-
if [ -n "$TRAVIS_TAG" ]; then
  export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$TRAVIS_TAG
else
  export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-${TRAVIS_COMMIT:0:7}
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
echo copy files
cd $TRAVIS_BUILD_DIR
mkdir docker.tmp
mkdir docker.tmp/bin
sudo cp $HOME/.local/bin/glob-launch docker.tmp/bin
sudo cp $TRAVIS_BUILD_DIR/integration/Dockerfiles/hub/Dockerfile docker.tmp
sudo cp $TRAVIS_BUILD_DIR/integration/ShellScript/entrypoint.sh docker.tmp
echo build docker
cd docker.tmp
docker build -t qinka/glob:$DOCKER_IMAGE_TAG .
docker build -t qinka/glob:$LATEST .
docker push  qinka/glob
