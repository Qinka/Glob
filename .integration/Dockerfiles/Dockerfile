# build
FROM index.docker.io/library/debian:latest
MAINTAINER qinka
RUN apt update && apt install -y libgmp10
ADD bin /usr/bin
EXPOSE 3000
CMD cat /cfg | /glob-launch
