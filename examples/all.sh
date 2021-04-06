#! /usr/bin/env bash

cd "$(dirname "$(readlink -f "$0")")"

./gitea/build.sh
./apache-runit/build.sh
./apache/build.sh

./gitea/run.sh
./apache-runit/run.sh
./apache/run.sh
