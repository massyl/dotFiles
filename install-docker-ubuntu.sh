#!/bin/bash

# add docker gpg key
sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
# Add Docker repository to source list
sudo touch /etc/apt/sources.list.d/docker.list
