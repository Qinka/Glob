FROM index.docker.io/library/haskell:7.10.3
MAINTAINER qinka
ADD .dist/bin/* /usr/local/bin
ADD ./resource/favicon.ico /favicon.ico
RUN rm -r src
EXPOSE 3000
