#! /usr/bin/env bash

cd "$(dirname "$(readlink -f "$0")")"

docker run -v $PWD/internal_token:/internal_token -v $PWD/jwt_secret:/jwt_secret -v $PWD/lfs_jwt_secret:/lfs_jwt_secret -v $PWD/secret_key:/secret_key -p 3000:3000 nixng &

DOCKER=$!

sleep 5
xdg-open http://localhost:3000/
read

kill -SIGTERM $DOCKER
