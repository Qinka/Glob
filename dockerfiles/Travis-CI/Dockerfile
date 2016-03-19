FROM index.docker.io/library/haskell:7.10.3
MAINTAINER qinka
ADD ./dist/build/glob/glob /usr/local/bin
ADD ./dist/build/launch.docker/launch.docker
ADD ./resource/favicon.ico /favicon.ico
RUN rm -r src
EXPOSE 3000
