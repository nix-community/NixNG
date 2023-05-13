# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, lib, nglib, config, ... }:
with lib;
{
  options.system.build = {
    runDocker = mkOption {
      description = ''
          A script which will run the system in docker using nix store bind mounts.
        '';
      type = types.path;
    };
  };

  config.system.build.runDocker =
    pkgs.writeShellScript "nixng-${config.system.name}-docker.run"
      (let
         closureInfo = pkgs.closureInfo
           { rootPaths = [ config.system.build.toplevel ]; };
       in
         ''
           docker run $(xargs -I{} printf -- ' --mount type=bind,source={},destination={},ro' < ${closureInfo}/store-paths) $DOCKER_OPTS magicrb/nix-container-base@sha256:01f199486f5b0e3c90411d700436395f21154f8234b6dfa86eb224eb5b6ad43b ${config.system.build.toplevel}/init "$@"
         '');
}

