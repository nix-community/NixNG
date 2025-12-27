# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{
  config,
  lib,
  pkgs,
  nglib,
  ...
}:
let
  cfg = config.security.ca;

  cacertPackage = pkgs.cacert.override { blacklist = cfg.certificateBlacklist; };

  caCertificates =
    pkgs.runCommand "ca-certificates.crt"
      {
        files = cfg.certificateFiles ++ [
          (builtins.toFile "extra.crt" (lib.concatStringsSep "\n" cfg.certificates))
        ];
        preferLocalBuild = true;
      }
      ''
        cat $files > $out
      '';
in
{
  options.security.ca = {
    certificateFiles = lib.mkOption {
      description = ''
        A list of files containing trusted root certificates in PEM
        format. These are concatenated to form
        <filename>/etc/ssl/certs/ca-certificates.crt</filename>, which is
        used by many programs that use OpenSSL, such as
        <command>curl</command> and <command>git</command>.
      '';
      type = with lib.types; listOf path;
      default = [ ];
    };

    certificates = lib.mkOption {
      description = ''
        A list of trusted root certificates in PEM format.
      '';
      type = with lib.types; listOf str;
      default = [ ];
    };

    certificateBlacklist = lib.mkOption {
      description = ''
        A list of blacklisted CA certificate names that won't be imported from
        the Mozilla Trust Store into
        <filename>/etc/ssl/certs/ca-certificates.crt</filename>. Use the
        names from that file.
      '';
      type = with lib.types; listOf str;
      default = [ ];
    };
  };

  config = {
    security.ca.certificateFiles = [ "${cacertPackage}/etc/ssl/certs/ca-bundle.crt" ];

    # NixOS canonical location + Debian/Ubuntu/Arch/Gentoo compatibility.
    environment.etc."ssl/certs/ca-certificates.crt" = {
      mode = "symlink";
      source = "${caCertificates}";
    };
    # CentOS/Fedora compatibility.
    environment.etc."pki/tls/certs/ca-bundle.crt" = {
      mode = "symlink";
      source = "${caCertificates}";
    };
  };
}
