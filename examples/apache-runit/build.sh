#! /usr/bin/env bash

cd "$(dirname "$(readlink -f "$0")")"

nix build --out-link ./nixng-apache-runit.tar.xz ../..#apacheRunitSystem.ociImage
docker load < ./nixng-apache-runit.tar.xz
