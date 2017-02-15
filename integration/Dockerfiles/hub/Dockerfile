# build
FROM index.docker.io/library/debian:latest
MAINTAINER qinka
RUN apt update && apt install -y libgmp10 python
ADD bin /usr/bin
COPY entrypoint.py entrypoint
ENTRYPOINT ["/entrypoint"]

