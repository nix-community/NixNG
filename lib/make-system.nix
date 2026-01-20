# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

# These arguments are provided by the overarching NixNG repository and are not user confugurable.
{ nglib, overlay }:
# These arguments are user configurable
{
  nixpkgs,
  name,
  system,
  config,
  defaultModules ? import ../modules/list.nix,
  extraModules ? [ ],
  specialArgs ? { },
}:
let
  inherit (nixpkgs.lib) evalModules filter concatStringsSep;

  evaledModules = evalModules {
    specialArgs = specialArgs // {
      inherit nglib;
    };

    modules =
      defaultModules
      ++ extraModules
      ++ [
        (
          { ... }:
          {
            system.name = name;
          }
        )
        config
        (
          { ... }:
          {
            nixpkgs.pkgs = nixpkgs.legacyPackages.${system};
            _module.args = {
              inherit system;
            };
          }
        )
      ];
  };

  failedAssertions = map (x: x.message) (filter (x: !x.assertion) evaledModules.config.assertions);
  configValid =
    if failedAssertions != [ ] then
      throw "\nFailed assertions:\n${concatStringsSep "\n" (map (x: "- ${x}") failedAssertions)}"
    else
      evaledModules.config;
in
evaledModules // { config = configValid; }
