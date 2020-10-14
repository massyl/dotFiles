
FROM  ubuntu:latest
MAINTAINER TOTO

ENV http_proxy 'http://127.0.0.1:3128'
ENV https_proxy 'http://127.0.0.1:3128'
ENV HTTP_PROXY 'http://127.0.0.1:3128'
ENV HTTPS_PROXY 'http://127.0.0.1:3128'

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10
RUN echo "deb http://repo.mongodb.org/apt/ubuntu "$(lsb_release -sc)"/mongodb-org/3.0 multiverse" | tee /etc/apt/sources.list.d/mongodb-org-3.0.list

#apt-get sources AND install MongoDB #
RUN apt-get update && apt-get install -y mongodb-org
RUN mkdir -p /data/db
EXPOSE 27017
ENTRYPOIN ["/usr/bin/mongod"]
