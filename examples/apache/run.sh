#! /usr/bin/env bash

cd "$(dirname "$(readlink -f "$0")")"

docker run -p 80:80 nixng-apache &
DOCKER=$!

sleep 5
xdg-open http://localhost/
read

kill -SIGTERM $DOCKER
