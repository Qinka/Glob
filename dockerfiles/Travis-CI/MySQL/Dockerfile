FROM index.docker.io/library/debian:latest
MAINTAINER qinka
RUN apt-get update && apt-get -y install libgmp10 libmysqlclient18
ADD dist/build/launch.docker /usr/bin
CMD launch.docker
EXPOSE 3000
