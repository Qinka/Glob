FROM index.docker.io/library/debian:latest
MAINTAINER qinka
RUN apt-get update && apt-get -y install libgmp10 libpq5
ADD ./dist/build/glob /usr/bin/
ADD ./dist/build/launch.docker /usr/bin
ADD ./resource/favicon.ico /favicon.ico
CMD launch.docker
EXPOSE 3000
