#!/bin/bash  -x

#
# install and configure consul
#
sudo mkdir -p /etc/consul.d/server
sudo mkdir -p /var/consul
sudo mkdir -p /var/www/html

curl -O -J -L https://dl.bintray.com/mitchellh/consul/0.5.2_linux_amd64.zip
sudo apt-get install unzip curl emacs -y && unzip 0.5.2_linux_amd64.zip && mv consul /usr/local/bin/
# nohup /usr/local/bin/consul agent -config-dir="/etc/consul.d/server/config.json" -ui-dir="/var/www/html" -advertise=$PUBLIC_IP -bind=$PRIVATE_IP >>/var/log/consul.log 2>&1 &

sleep 2

# Install Docker
sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
sudo tee -a /etc/apt/sources.list.d/docker.list << EOF
deb https://apt.dockerproject.org/repo ubuntu-precise main
EOF

sudo apt-get update
sudo apt-get purge lxc-docker
sudo apt-get install docker-engine -y
