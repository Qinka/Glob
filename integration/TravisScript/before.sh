#!/bin/bash
echo pre-install
########
echo update apt
sudo apt update
sudo apt install wget
######
echo fetch the system\' name
export OS_CORENAME=$(lsb_release -a | grep Codename | awk '{print $2}')
export OS_DISTRIBUTOR=$(lsb_release -a | grep Descriptino | awk '{print $2}')
echo using $OS_DISTRIBUTOR  $OS_CORENAME
######
if [ -n "$LLVM" ]; then
  echo install llvm
  wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
  echo deb http://apt.llvm.org/$OS_CORENAME/ llvm-toolchain-$OS_CORENAME main | sudo tee -a /etc/apt/sources.list.d/llvm.list
  echo deb-src http://apt.llvm.org/$OS_CORENAME/ llvm-toolchain-$OS_CORENAME main | sudo tee -a /etc/apt/sources.list.d/llvm.list
  echo deb http://apt.llvm.org/$OS_CORENAME/ llvm-toolchain-$OS_CORENAME-$LLVM main | sudo tee -a /etc/apt/sources.list.d/llvm.list
  echo deb-src http://apt.llvm.org/$OS_CORENAME/ llvm-toolchain-$OS_CORENAME-$LLVM main | sudo tee -a /etc/apt/sources.list.d/llvm.list
  sudo apt update
  sudo apt install -y libllvm-$LLVM-ocaml-dev libllvm$LLVM libllvm$LLVM-dbg lldb-$LLVM llvm-$LLVM llvm-$LLVM-dev llvm-$LLVM-runtime lldb-$LLVM-dev
else
  echo without llvm
fi
######
echo login docker
docker login -e="$DOCKER_EMAIL" -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"
######
echo install stack
mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH
travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
echo setting up ghc
export PATH=/opt/ghc/$GHC_VER/bin:$PATH
######
