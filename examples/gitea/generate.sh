#! /usr/bin/env bash

if ! which gitea > /dev/null ; then
    printf 'Please enter an environment where `gitea` is available!'
    exit 1
fi

rm -v internal_token jwt_secret lfs_jwt_secret secret_key 

gitea generate secret INTERNAL_TOKEN > internal_token
gitea generate secret JWT_SECRET > jwt_secret
gitea generate secret JWT_SECRET > lfs_jwt_secret
gitea generate secret SECRET_KEY > secret_key
