#!/bin/bash
echo update
############
export DOCKER_IMAGE_TAG=docekr-
if [ -n "$TRAVIS_TAG" ]; then
  export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$TRAVIS_TAG
else
  export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-${TRAVIS_COMMIT:0:7}
fi
export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$(uname)-$OS_DISTRIBUTOR-$OS_CORENAME-GHC_$GHCVER-$(uname -m)
if [ -n "$LLVM" ]; then
  export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-llvm-$LLVM
fi
if [ -n "$THREADED" ]; then
  export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-threaded
fi
echo copy files
cd $TRAVIS_BUILD_DIR
mkdir docker.tmp
mkdir docker.tmp/bin
sudo cp $HOME/.local/bin/glob-launch docker.tmp/bin
sudo cp $TRAVIS_BUILD_DIR/integration/Dockerfiles/hub/Dockerfile docker.tmp
sudo cp $TRAVIS_BUILD_DIR/integration/ShellScript/start.sh docker.tmp/bin
echo build docker
cd docker.tmp
docker build -t qinka/glob:$DOCKER_IMAGE_TAG .
docker push  qinka/glob
