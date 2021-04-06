#! /usr/bin/env bash

cd "$(dirname "$(readlink -f "$0")")"

echo "apache-runit is no going be run since runit still refuses to cooperate!"

#docker run -p 80:80 nixng-apache-runit &
#DOCKER=$!

#sleep 5
#xdg-open http://localhost/
#read

#kill -SIGTERM $DOCKER
