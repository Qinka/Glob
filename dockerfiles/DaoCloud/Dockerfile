FROM index.docker.io/library/haskell:7.10.2
MAINTAINER qinka
ADD . /src
RUN cp /src/resource/favicon.ico /favicon.ico
RUN echo "image/x-ico" > /favicon.ico.mime
RUN cd /src && cabal sandbox init
RUN apt-get update && apt-get -y install wget
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ trusty-pgdg main" > /etc/apt/sources.list.d/pgdg.list
RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
RUN apt-get update && apt-get -y install postgresql-server-dev-9.4
RUN cd /src && cabal update && cabal install -j9 directory process persistent-template yesod persistent-postgresql warp-tls cmdargs
RUN cd /src && cabal install glob-auth/
RUN cd /src && cabal install glob-core/ -fwith-postgresql
RUN cd /src && cabal install glob-main/
RUN cd /src && cabal install glob-launcher/ -flaunch-docker -fwith-postgresql
RUN cp /src/.cabal-sandbox/bin/* /usr/bin
RUN rm -r src
EXPOSE 3000
