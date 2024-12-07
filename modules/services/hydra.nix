# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.
#
# This file incorporates work sublicensed from the MIT License to
# Mozilla Public License, v. 2.0, for which the following copyright applies:
#   Copyright (c) 2003-2021 Eelco Dolstra and the Nixpkgs/NixOS contributors

{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.services.hydra;

  # BEGIN Copyright (c) 2003-2021 Eelco Dolstra and the Nixpkgs/NixOS contributors
  baseDir = "/var/lib/hydra";

  hydraEnv = {
    HYDRA_CONFIG = "${baseDir}/hydra.conf";
    HYDRA_DATA = "${baseDir}";
  };

  env =
    {
      NIX_REMOTE = "daemon";
      SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt";
      PGPASSFILE = "${baseDir}/pgpass";
      # NIX_REMOTE_SYSTEMS = concatStringsSep ":" cfg.buildMachinesFiles;
    }
    // lib.optionalAttrs (cfg.smtpHost != null) {
      EMAIL_SENDER_TRANSPORT = "SMTP";
      EMAIL_SENDER_TRANSPORT_host = cfg.smtpHost;
    }
    // hydraEnv
    // cfg.extraEnv;

  serverEnv =
    env
    // {
      HYDRA_TRACKER = cfg.tracker;
      XDG_CACHE_HOME = "${baseDir}/www/.cache";
      COLUMNS = "80";
      PGPASSFILE = "${baseDir}/pgpass-www"; # grrr
    }
    // (lib.optionalAttrs cfg.debugServer { DBIC_TRACE = "1"; });

  localDB = "dbi:Pg:dbname=hydra;user=hydra;";

  haveLocalDB = cfg.dbiFile == null;

  hydra-package =
    let
      makeWrapperArgs = lib.concatStringsSep " " (
        lib.mapAttrsToList (key: value: "--set \"${key}\" \"${value}\"") hydraEnv
      );
    in
    pkgs.buildEnv rec {
      name = "hydra-env";
      buildInputs = [ pkgs.makeWrapper ];
      paths = [ cfg.package ];

      postBuild = ''
        if [ -L "$out/bin" ]; then
            unlink "$out/bin"
        fi
        mkdir -p "$out/bin"
        for path in ${lib.concatStringsSep " " paths}; do
          if [ -d "$path/bin" ]; then
            cd "$path/bin"
            for prg in *; do
              if [ -f "$prg" ]; then
                rm -f "$out/bin/$prg"
                if [ -x "$prg" ]; then
                  makeWrapper "$path/bin/$prg" "$out/bin/$prg" ${makeWrapperArgs} \
                    ${
                      if cfg.dbiFile == null then
                        ''--set HYDRA_DBI '${localDB}' ''
                      else
                        ''--run 'export HYDRA_DBI=$(cat "${cfg.dbiFile}")' ''
                    }
                fi
              fi
            done
          fi
        done
      '';
    };
  # END Copyright (c) 2003-2021 Eelco Dolstra and the Nixpkgs/NixOS contributors

  parser =
    let
      valToString =
        v:
        if lib.isString v then
          "${v}"
        else if lib.isInt v then
          "${toString v}"
        else if lib.isBool v then
          if v then "true" else "false"
        else if lib.isList v then
          lib.concatMapStringsSep " " (x: valToString x) v
        else
          abort "Invalid config, module system should have caught this!";
    in
    config: lib.concatStringsSep "\n" (lib.mapAttrsToList (n: v: "${n} = ${valToString v}") config);
in
{
  options.services.hydra = {
    enable = lib.mkEnableOption "Enable HydraCI";

    # BEGIN Copyright (c) 2003-2021 Eelco Dolstra and the Nixpkgs/NixOS contributors
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.hydra-unstable;
      description = ''
        Which HydraCI package to use.
      '';
    };

    hydraURL = lib.mkOption {
      type = lib.types.str;
      description = ''
        The base URL for the Hydra webserver instance. Used for links in emails.
      '';
    };

    listenHost = lib.mkOption {
      type = lib.types.str;
      default = "*";
      example = "localhost";
      description = ''
        The hostname or address to listen on or <literal>*</literal> to listen
        on all interfaces.
      '';
    };

    port = lib.mkOption {
      type = lib.types.int;
      default = 3000;
      description = ''
        TCP port the web server should listen to.
      '';
    };

    minimumDiskFree = lib.mkOption {
      type = lib.types.int;
      default = 0;
      description = ''
        Threshold of minimum disk space (GiB) to determine if the queue runner should run or not.
      '';
    };

    minimumDiskFreeEvaluator = lib.mkOption {
      type = lib.types.int;
      default = 0;
      description = ''
        Threshold of minimum disk space (GiB) to determine if the evaluator should run or not.
      '';
    };

    notificationSender = lib.mkOption {
      type = lib.types.str;
      description = ''
        Sender email address used for email notifications.
      '';
    };

    smtpHost = lib.mkOption {
      type = with lib.types; nullOr str;
      default = null;
      example = [ "localhost" ];
      description = ''
        Hostname of the SMTP server to use to send email.
      '';
    };

    tracker = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''
        Piece of HTML that is included on all pages.
      '';
    };

    logo = lib.mkOption {
      type = with lib.types; nullOr path;
      default = null;
      description = ''
        Path to a file containing the logo of your Hydra instance.
      '';
    };

    debugServer = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether to run the server in debug mode.";
    };

    extraEnv = lib.mkOption {
      type = with lib.types; attrsOf str;
      default = { };
      description = "Extra environment variables for Hydra.";
    };

    gcRootsDir = lib.mkOption {
      type = lib.types.path;
      default = "/nix/var/nix/gcroots/hydra";
      description = "Directory that holds Hydra garbage collector roots.";
    };

    buildMachinesFiles = lib.mkOption {
      type = with lib.types; listOf path;
      default = "/etc/nix/machines"; # optional (config.nix.buildMachines != [])
      example = [
        "/etc/nix/machines"
        "/var/lib/hydra/provisioner/machines"
      ];
      description = "List of files containing build machines.";
    };

    useSubstitutes = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Whether to use binary caches for downloading store paths. Note that
        binary substitutions trigger (a potentially large number of) additional
        HTTP requests that slow down the queue monitor thread significantly.
        Also, this Hydra instance will serve those downloaded store paths to
        its users with its own signature attached as if it had built them
        itself, so don't enable this feature unless your active binary caches
        are absolute trustworthy.
      '';
    };
    # END Copyright (c) 2003-2021 Eelco Dolstra and the Nixpkgs/NixOS contributors

    dbiFile = lib.mkOption {
      type = with lib.types; nullOr str;
      default = null;
      description = ''
        If set to <literal>null</literal>, then a local PostgreSQL instance will
        be setup and used, when set to a string containing the path to a file,
        containing one line with a dbi, that dbi will be used and no local
        database will be setup.
      '';
    };

    adjustNiceness = lib.mkOption {
      type = with lib.types; bool;
      default = false;
      description = ''
        Whether to adjust the process priority of the Hydra evaluator and queue
        runner in order for the web UI to always stay responsive. This will also
        help other parts of your system.
      '';
    };

    config = lib.mkOption {
      type =
        with lib.types;
        attrsOf (oneOf [
          int
          bool
          str
          (listOf (oneOf [
            int
            bool
            str
          ]))
        ]);
      description = ''
        Hydra configuration
      '';
      apply = x: pkgs.writeScript "hydra.conf" (parser x);
      default = { };
    };
  };

  config = lib.mkIf cfg.enable {
    users.groups.hydra = {
      gid = config.ids.gids.hydra;
    };

    users.users.hydra = {
      description = "Hydra";
      group = "hydra";
      createHome = true;
      home = baseDir;
      useDefaultShell = true;
      uid = config.ids.uids.hydra;
    };

    users.users.hydra-queue-runner = {
      description = "Hydra queue runner";
      group = "hydra";
      useDefaultShell = true;
      home = "${baseDir}/queue-runner"; # really only to keep SSH happy
      uid = config.ids.uids.hydra-queue-runner;
    };

    users.users.hydra-www = {
      description = "Hydra web server";
      group = "hydra";
      useDefaultShell = true;
      uid = config.ids.uids.hydra-www;
    };

    nix = {
      daemon = true;
      enable = true;
      config = {
        keep-outputs = true;
        keep-derivations = true;

        gc-check-reachability = false;

        trusted-users = [ "hydra-queue-runner" ];
      };
    };

    services.hydra.config = {
      using_frontend_proxy = true;
      base_uri = cfg.hydraURL;
      notification_sender = cfg.notificationSender;
      max_servers = 25;
      hydra_logo = lib.mkIf (cfg.logo != null) cfg.logo;
      gc_roots_dir = cfg.gcRootsDir;
      use-substitutes = cfg.useSubstitutes;
    };

    services.crond = {
      enable = true;

      crontabs = {
        hydra-update-gc-roots = {
          environment = env;
          jobs =
            let
              hydra-update-gc-roots = pkgs.writeShellScript "hydra-update-gc-roots" ''
                export PATH=${
                  lib.makeBinPath [
                    pkgs.busybox
                    pkgs.runit
                  ]
                }:$PATH

                sv -c -w 0 once hydra-init
                while [[ ! -e ${baseDir}/.init-hydra ]]; do
                  sleep 1
                done

                ${hydra-package}/bin/hydra-update-gc-roots hydra-update-gc-roots
              '';
            in
            [ "15 2,14 * * * hydra ${hydra-update-gc-roots}" ];
        };

        hydra-support = {
          jobs =
            let
              hydra-check-space = pkgs.writeShellScript "hydra-check-space" ''
                export PATH=${
                  lib.makeBinPath [
                    pkgs.busybox
                    pkgs.runit
                  ]
                }:$PATH

                if [ $(($(stat -f -c '%a' /nix/store) * $(stat -f -c '%S' /nix/store))) -lt $((${toString cfg.minimumDiskFree} * 1024**3)) ]; then
                  echo "stopping Hydra queue runner due to lack of free space..."
                  sv stop hydra-queue-runner
                fi
                if [ $(($(stat -f -c '%a' /nix/store) * $(stat -f -c '%S' /nix/store))) -lt $((${toString cfg.minimumDiskFreeEvaluator} * 1024**3)) ]; then
                  echo "stopping Hydra evaluator due to lack of free space..."
                  sv stop hydra-evaluator
                fi
              '';
              hydra-compress-logs = pkgs.writeShellScript "hydra-compress-logs" ''
                export PATH=${
                  lib.makeBinPath [
                    pkgs.bzip2
                    pkgs.busybox
                  ]
                }:$PATH
                find /var/lib/hydra/build-logs -type f -name "*.drv" -mtime +3 -size +0c | xargs -r bzip2 -v -f
              '';
            in
            [
              "*/2 * * * * root ${hydra-check-space}"
              "45 01 * * 7 root ${hydra-compress-logs}"
            ];
        };
      };
    };

    environment.systemPackages = [ cfg.package ];
    environment.variables = hydraEnv;

    init.services = {
      hydra-init = {
        environment = env;
        script = pkgs.writeShellScript "hydra-init" ''
          ln -sf ${cfg.config} ${baseDir}/hydra.conf

          [[ -e ${baseDir}/.init-hydra ]] && exit 0

          mkdir -p ${baseDir}
          chown hydra.hydra ${baseDir}
          chmod 0750 ${baseDir}

          ${lib.optionalString haveLocalDB "sv -v -w 0 up postgresql"}

          mkdir -m 0700 -p ${baseDir}/www
          chown hydra-www.hydra ${baseDir}/www
          mkdir -m 0700 -p ${baseDir}/queue-runner
          mkdir -m 0750 -p ${baseDir}/build-logs
          chown hydra-queue-runner.hydra ${baseDir}/queue-runner ${baseDir}/build-logs

          mkdir -pm 2775 ${cfg.gcRootsDir}
          chown hydra.hydra ${cfg.gcRootsDir}

          export PATH=${pkgs.nettools}/bin:$PATH # Hydra runs some variant of `hostname --fqdn`, which BusyBox doesn't support
          HOME=~hydra chpst -u hydra:hydra ${hydra-package}/bin/hydra-init || exit 1
          touch ${baseDir}/.init-hydra
        '';
      };

      hydra-server =
        let
          hydraCmd =
            "${hydra-package}/bin/hydra-server hydra-server -f -h '${cfg.listenHost}' "
            + "-p ${toString cfg.port} --max_spare_servers 5 --max_servers 25 "
            + "--max_requests 100 ${lib.optionalString cfg.debugServer "-d"}";
        in
        {
          environment = serverEnv;
          pwd = "${baseDir}/queue-runner";
          script = pkgs.writeShellScript "hydra-server" ''
            sv -v -w 0 once hydra-init
            [[ ! -e ${baseDir}/.init-hydra ]] && exit 1

            export PATH=${pkgs.nettools}/bin:$PATH # Hydra runs some variant of `hostname --fqdn`, which BusyBox doesn't support
            HOME=~hydra-www exec chpst -u hydra-www:hydra ${hydraCmd}
          '';
          enabled = true;
        };

      hydra-queue-runner = {
        environment = env // {
          PGPASSFILE = "${baseDir}/pgpass-queue-runner"; # grrr
          IN_SYSTEMD = "0"; # to get log severity levels
        };
        pwd = "${baseDir}/queue-runner";
        script = pkgs.writeShellScript "hydra-queue-runner" ''
          export PATH=${
            lib.makeBinPath [
              hydra-package
              pkgs.nettools
              pkgs.openssh
              pkgs.bzip2
              config.nix.package
            ]
          }:$PATH

          sv -v -w 0 once hydra-init
          [[ ! -e ${baseDir}/.init-hydra ]] && exit 1

          export PATH=${pkgs.nettools}/bin:$PATH # Hydra runs some variant of `hostname --fqdn`, which BusyBox doesn't support

          HOME=~hydra-queue-runner LOGNAME=hydra-queue-runner chpst ${lib.optionalString cfg.adjustNiceness "-n +5"} -u hydra-queue-runner:hydra ${hydra-package}/bin/hydra-queue-runner -v
          HOME=~hydra-queue-runner LOGNAME=hydra-queue-runner chpst ${lib.optionalString cfg.adjustNiceness "-n +5"} -u hydra-queue-runner:hydra ${hydra-package}/bin/hydra-queue-runner --unlock
        '';
        enabled = true;
      };

      hydra-evaluator = {
        environment = env;
        pwd = baseDir;
        script = pkgs.writeShellScript "hydra-evaluator" ''
          export PATH=${
            with pkgs;
            lib.makeBinPath [
              hydra-package
              nettools
              jq
            ]
          }:$PATH

          sv -v -w 0 once hydra-init
          [[ ! -e ${baseDir}/.init-hydra ]] && exit 1

          export PATH=${pkgs.nettools}/bin:$PATH # Hydra runs some variant of `hostname --fqdn`, which BusyBox doesn't support
          HOME=~hydra exec chpst ${lib.optionalString cfg.adjustNiceness "-n +5"} -u hydra:hydra ${hydra-package}/bin/hydra-evaluator
        '';
        enabled = true;
      };
    };

    # BEGIN Copyright (c) 2003-2021 Eelco Dolstra and the Nixpkgs/NixOS contributors
    services.postgresql.enable = lib.mkIf haveLocalDB true;

    services.postgresql.identMap = lib.optionalString haveLocalDB ''
      hydra-users hydra hydra
      hydra-users hydra-queue-runner hydra
      hydra-users hydra-www hydra
      hydra-users root hydra
      # The postgres user is used to create the pg_trgm extension for the hydra database
      hydra-users postgres postgres
    '';

    services.postgresql.authentication = lib.optionalString haveLocalDB ''
      local hydra all ident map=hydra-users
    '';
    # END Copyright (c) 2003-2021 Eelco Dolstra and the Nixpkgs/NixOS contributors

    services.postgresql.ensureDatabases = lib.mkIf haveLocalDB [ "hydra" ];
    services.postgresql.ensureExtensions = lib.mkIf haveLocalDB { "pg_trgm" = [ "hydra" ]; };
    services.postgresql.ensureUsers = lib.mkIf haveLocalDB [
      {
        name = "hydra";
        ensurePermissions = {
          "DATABASE \"hydra\"" = "ALL PRIVILEGES";
        };
      }
    ];
  };
}
