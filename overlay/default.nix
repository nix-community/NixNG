# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

final: prev:
let
  inherit (final) haskellPackages;
  inherit (prev) callPackage;
in
{
  tinyLinux = callPackage ./tiny-linux.nix { };
  runVmLinux = final.callPackage ./run-vm-linux.nix { };
  cronie = callPackage ./cronie.nix { };
  pause = callPackage ./pause.nix { };
  sigell = haskellPackages.callPackage ./sigell/cabal.nix { };
  systemdStandalone = callPackage ./systemd-minimal.nix { };
  systemdTmpfilesD = callPackage ./systemd-tmpfiles.d.nix { };
  dinit = callPackage ./dinit.nix { };

  util-linuxSystemdFree = prev.util-linux.override {
    systemdSupport = false;
    pamSupport = true;
  };
  runit = prev.runit.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs or [ ] ++ (with prev; [ makeWrapper ]);
    fixupPhase = old.fixupPhase or "" + ''
      wrapProgram $out/bin/sv \
        --set SVDIR "/service/"
    '';
  });

  inherit (callPackage ./trivial-builders.nix { })
    writeSubstitutedFile
    writeSubstitutedShellScript
    writeSubstitutedShellScriptBin
    ;

  setgroups = prev.writeCBin "setgroups" (builtins.readFile ./setgroups.c);

  fileHammer = prev.haskell.lib.enableSeparateBinOutput (
    prev.haskellPackages.callPackage ./haskell/file-hammer/package.nix { }
  );

  # inherit
  #   (nixpkgsTrivialBuilders)
  #   writeShellScript
  #   writeShellScriptBin
  #   writeShellScriptApplication;
}
