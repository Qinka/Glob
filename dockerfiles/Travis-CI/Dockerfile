FROM index.docker.io/library/debian:latest
MAINTAINER qinka
ADD ./dist/build/glob /usr/bin/
ADD ./dist/build/launch.docker /usr/bin
ADD ./resource/favicon.ico /favicon.ico
RUN rm -r src
EXPOSE 3000
