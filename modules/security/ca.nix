/*
  * NixNG
  * Copyright (c) 2021  GPL Magic_RB <magic_rb@redalder.org>
  *
  *  This file is free software: you may copy, redistribute and/or modify it
  *  under the terms of the GNU General Public License as published by the
  *  Free Software Foundation, either version 3 of the License, or (at your
  *  option) any later version.
  *
  *  This file is distributed in the hope that it will be useful, but
  *  WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  *  General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

{ config, lib, pkgs, nglib, ... }:
with lib;
let
  cfg = config.security.ca;

  cacertPackage = pkgs.cacert.override {
    blacklist = cfg.certificateBlacklist;
  };

  caCertificates = pkgs.runCommand "ca-certificates.crt"
    {
      files =
        cfg.certificateFiles ++
        [ (builtins.toFile "extra.crt" (concatStringsSep "\n" cfg.certificates)) ];
      preferLocalBuild = true;
    }
    ''
      cat $files > $out
    '';
in
{
  options.security.ca = {
    certificateFiles = mkOption {
      description = ''
        A list of files containing trusted root certificates in PEM
        format. These are concatenated to form
        <filename>/etc/ssl/certs/ca-certificates.crt</filename>, which is
        used by many programs that use OpenSSL, such as
        <command>curl</command> and <command>git</command>.
      '';
      type = with types; listOf path;
      default = [ ];
    };

    certificates = mkOption {
      description = ''
        A list of trusted root certificates in PEM format.
      '';
      type = with types; listOf str;
      default = [ ];
    };

    certificateBlacklist = mkOption {
      description = ''
        A list of blacklisted CA certificate names that won't be imported from
        the Mozilla Trust Store into
        <filename>/etc/ssl/certs/ca-certificates.crt</filename>. Use the
        names from that file.
      '';
      type = with types; listOf str;
      default = [ ];
    };
  };

  config = {
    security.ca.certificateFiles = [ "${cacertPackage}/etc/ssl/certs/ca-bundle.crt" ];

    system.activation.cacerts = nglib.dag.dagEntryAnywhere ''
      export PATH=${pkgs.busybox}/bin

      # NixOS canonical location + Debian/Ubuntu/Arch/Gentoo compatibility.
      mkdir -m 0755 -p /etc/ssl/certs
      ln -sfn ${caCertificates} /etc/ssl/certs/.ca-certificates.crt.tmp
      mv /etc/ssl/certs/.ca-certificates.crt.tmp /etc/ssl/certs/ca-certificates.crt # atomically replace /etc/ssl/certs/ca-certificates.crt

      # CentOS/Fedora compatibility.
      mkdir -m 0755 -p /etc/pki/tls/certs
      ln -sfn ${caCertificates} /etc/pki/tls/certs/.ca-bundle.crt.tmp
      mv /etc/pki/tls/certs/.ca-bundle.crt.tmp /etc/pki/tls/certs/ca-bundle.crt # atomically replace /etc/pki/tls/certs/ca-bundle.crt
    '';
  };
}
