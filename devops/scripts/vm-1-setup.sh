#!/bin/bash -x

# SCRIPTS=`pwd`
# source $SCRIPTS/bin/01-common-setup.sh

echo "Current HOST_IP"
echo $HOST_IP

echo "Consul cluster "
echo $CONSUL_CLUSTER_IP

#Join  Consul Cluster
docker run --rm progrium/consul cmd:run $CONSUL_CLUSTER_IP -d -v /mnt:/data

# Run registrator on each VM
docker run -d -v /var/run/docker.sock:/tmp/docker.sock --name registrator -h registrator  gliderlabs/registrator:latest consul://$HOST_IP:8500
