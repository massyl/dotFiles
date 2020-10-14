#!/bin/sh
# example: ./push.sh dtldev07@dtldevtt229.fr.world.socgen


ssh -i ~/.ssh/id_rsa.pub $1 "mkdir ~/.deployment"
scp -i ~/.ssh/id_rsa.pub install.sh mongodb.conf setReplicaSet.sh $1:~/.deployment
ssh -i ~/.ssh/id_rsa.pub $1 "chmod +x ~/.deployment/install.sh"

# ./mongod --config /home/dtldev07/homeware/mongo/conf/mongodb.conf
# ./mongod --config /home/dtldev07/homeware/mongo/conf/mongodb-arbiter.conf

scp -i ~/.ssh/id_rsa.pub install.sh mongodb.conf setReplicaSet.sh dtldev07@dtldevtt147.fr.world.socgen:~/.deployment
scp -i ~/.ssh/id_rsa.pub mongodb-arbiter.conf dtldev07@dtldevtt147.fr.world.socgen:~/homeware/mongo/conf
