# build
FROM index.docker.io/library/debian:latest
MAINTAINER qinka
RUN apt update && apt install -y libgmp10 wget curl jq
ADD bin /usr/bin
CMD start.sh fromenv
EXPOSE 3000
