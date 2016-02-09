#!/bin/bash  -x

SCRIPTS=`pwd`
source $SCRIPTS/bin/00-bootstrap.sh

export HOST_IP=$(ifconfig eth1 | grep 'inet ' | awk -F ":"  '/1/{ print $2 }' |awk '{print $1}' |xargs)
export CONSUL_CLUSTER_IP="172.28.128.3"

# Querying Consult for all running services
alias ls-services="curl 172.28.128.3:8500/v1/catalog/services"

#curl 172.28.128.3:8500/v1/catalog/service/python-micro-service

tee -a $SCRIPTS/.bashrc <<EOF
 sudo gpasswd -a ${USER} docker
EOF

source $SCRIPTS/.bashrc
