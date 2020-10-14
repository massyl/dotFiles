#!/bin/sh


APPSDIR=~/homeware

mkdir -p $APPSDIR

# Mongo environment setup

MONGOAPPDIR=$APPSDIR/mongo

MONGODIR=$MONGOAPPDIR/app
export MONGODATADIR=$MONGOAPPDIR/data>>~/.bashrc
export MONGOLOG=$MONGOAPPDIR/log>>~/.bashrc
export MONGOCONF=$MONGOAPPDIR/conf>>~/.bashrc

mkdir -p $MONGODIR
mkdir -p $MONGODATADIR
mkdir -p $MONGOLOG
mkdir -p $MONGOCONF

# Mongo install

wget http://srvclddtld006.fr.world.socgen:8000/mongodb-linux-x86_64-rhel62-3.0.6.tgz
tar -zxvf mongodb-linux-x86_64-rhel62-3.0.6.tgz --directory /tmp
mv /tmp/mongodb-linux-x86_64-rhel62-3.0.6/* $MONGODIR
export PATH=$MONGODIR/bin:$PATH>>~/.bashrc

cp mongodb.conf $MONGOCONF
