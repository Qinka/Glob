#!/bin/bash
echo update
############
export DOCKER_IMAGE_TAG=docekr_$TRAVIS_BUILD_NUMBER
if [ -n "$TRAVIS_TAG" ]; then
  export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$TRAVIS_TAG
else
  export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-${TRAVIS_COMMIT:0:7}
fi
export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$(uname)_$OS_DISTRIBUTOR\_$OS_CORENAME-GHC_$GHCVER-$(lscpu | grep Architecture | awk '{print $2}')
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
pwd
ls
ls bin
docker build -t qinka/glob:$DOCKER_IMAGE_TAG .
docker push  qinka/glob
