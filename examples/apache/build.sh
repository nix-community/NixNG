#! /usr/bin/env bash

cd "$(dirname "$(readlink -f "$0")")"

nix build --out-link ./nixng-apache.tar.xz ../..#apacheSystem.ociImage
docker load < ./nixng-apache.tar.xz
