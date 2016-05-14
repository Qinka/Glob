FROM index.docker.io/library/nginx:latest
MAINTAINER qinka
RUN apt-get update && apt-get -y install libgmp10 libpq5
ADD dist/build/launch.docker /usr/bin
ADD shell                    /usr/bin
CMD run.sh
EXPOSE 80
