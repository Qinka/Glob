# build
FROM index.docker.io/library/debian:latest
MAINTAINER qinka
RUN apt update && apt install -y libgmp10 wget curl jq
ADD bin /usr/bin
COPY entrypoint.sh entrypoint
ENTRYPOINT ["/entrypoint"]
#CMD start.sh fromenv
EXPOSE 3000
