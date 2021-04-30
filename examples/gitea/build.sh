#! /usr/bin/env bash

cd "$(dirname "$(readlink -f "$0")")"

nix build ../..#giteaSystem.config.system.build.ociImage.stream
./result | docker load
