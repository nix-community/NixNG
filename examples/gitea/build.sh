#! /usr/bin/env bash

cd "$(dirname "$(readlink -f "$0")")"

nix build --out-link ./nixng-gitea.tar.xz ../..#giteaSystem.ociImage
docker load < ./nixng-gitea.tar.xz
