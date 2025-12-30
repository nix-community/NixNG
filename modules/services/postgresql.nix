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
  cfg = config.services.postgresql;

  # BEGIN Copyright (c) 2003-2021 Eelco Dolstra and the Nixpkgs/NixOS contributors
  toStr =
    value:
    if true == value then
      "yes"
    else if false == value then
      "no"
    else if lib.isString value then
      "'${lib.replaceStrings [ "'" ] [ "''" ] value}'"
    else
      toString value;

  configFile = pkgs.writeTextDir "postgresql.conf" (
    lib.concatStringsSep "\n" (lib.mapAttrsToList (n: v: "${n} = ${toStr v}") cfg.config)
  );
in
{
  options = {
    services.postgresql = {

      enable = lib.mkEnableOption "PostgreSQL Server";

      package = lib.mkOption {
        type = lib.types.package;
        example = lib.literalExample "pkgs.postgresql_11";
        description = ''
          PostgreSQL package to use.
        '';
      };

      port = lib.mkOption {
        type = lib.types.int;
        default = 5432;
        description = ''
          The port on which PostgreSQL listens.
        '';
        apply = toString;
      };

      checkConfig = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Check the syntax of the configuration file at compile time";
      };

      dataDir = lib.mkOption {
        type = lib.types.path;
        defaultText = "/var/lib/postgresql/\${config.services.postgresql.package.psqlSchema}";
        example = "/var/lib/postgresql/11";
        description = ''
          The data directory for PostgreSQL. If left as the default value
          this directory will automatically be created before the PostgreSQL server starts, otherwise
          the sysadmin is responsible for ensuring the directory exists with appropriate ownership
          and permissions.
        '';
      };

      authentication = lib.mkOption {
        type = lib.types.lines;
        default = "";
        description = ''
          Defines how users authenticate themselves to the server. See the
          <link xlink:href="https://www.postgresql.org/docs/current/auth-pg-hba-conf.html">
          PostgreSQL documentation for pg_hba.conf</link>
          for details on the expected format of this option. By default,
          peer based authentication will be used for users connecting
          via the Unix socket, and md5 password authentication will be
          used for users connecting via TCP. Any added rules will be
          inserted above the default rules. If you'd like to replace the
          default rules entirely, you can use <function>lib.mkForce</function> in your
          module.
        '';
      };

      identMap = lib.mkOption {
        type = lib.types.lines;
        default = "";
        description = ''
          Defines the mapping from system users to database users.
          The general form is:
          map-name system-username database-username
        '';
      };

      initdbArgs = lib.mkOption {
        type = with lib.types; listOf str;
        default = [ ];
        example = [
          "--data-checksums"
          "--allow-group-access"
        ];
        description = ''
          Additional arguments passed to <literal>initdb</literal> during data dir
          initialisation.
        '';
      };

      initialScript = lib.mkOption {
        type =
          with lib.types;
          nullOr (oneOf [
            package
            str
          ]);
        default = null;
        description = ''
          A file containing SQL statements to execute on first startup.
        '';
      };

      startScript = lib.mkOption {
        type = lib.types.lines;
        description = ''
          PostgresSQL script to run on every startup.
        '';
        default = "";
      };

      earlyStartScript = lib.mkOption {
        type = lib.types.lines;
        description = ''
          PostgresSQL script to run on every startup, very early.
        '';
        default = "";
      };


      ensureExtensions = lib.mkOption {
        type = with lib.types; attrsOf (listOf str);
        default = { };
        description = ''
          Ensure that the specified extensions exist.
        '';
        example = {
          "pg_trgm" = [ "hydra" ];
        };
      };

      ensureDatabases = lib.mkOption {
        type =
          with lib.types;
          oneOf [
            (listOf str)
            (attrsOf (attrsOf str))
          ];
        default = [ ];
        description = ''
          Ensures that the specified databases exist.
          This option will never delete existing databases, especially not when the value of this
          option is changed. This means that databases created once through this option or
          otherwise have to be removed manually.
        '';
        example = lib.literalExample ''
          [
            "gitea" = { encoding = UTF-8; }
            "hydra"
          ]
        '';
      };

      ensureUsers = lib.mkOption {
        type =
          with lib.types;
          listOf (submodule {
            options = {
              name = lib.mkOption {
                type = str;
                description = ''
                  Name of the user to ensure.
                '';
              };
              ensurePermissions = lib.mkOption {
                type = attrsOf str;
                default = { };
                description = ''
                  Permissions to ensure for the user, specified as an attribute set.
                  The attribute names specify the database and tables to grant the permissions for.
                  The attribute values specify the permissions to grant. You may specify one or
                  multiple comma-separated SQL privileges here.
                  For more information on how to specify the target
                  and on which privileges exist, see the
                  <link xlink:href="https://www.postgresql.org/docs/current/sql-grant.html">GRANT syntax</link>.
                  The attributes are used as <code>GRANT ''${attrName} ON ''${attrValue}</code>.
                '';
                example = lib.literalExample ''
                  {
                    "DATABASE \"nextcloud\"" = "ALL PRIVILEGES";
                    "ALL TABLES IN SCHEMA public" = "ALL PRIVILEGES";
                  }
                '';
              };
              ensureDBOwnership = lib.mkOption {
                type = bool;
                default = false;
                description = lib.mdDoc ''
                  Grants the  user ownership to a database with the same name.
                  This database must be defined manually in
                  [](#opt-services.postgresql.ensureDatabases).
                '';
              };
            };
          });
        default = [ ];
        description = ''
          Ensures that the specified users exist and have at least the ensured permissions.
          The PostgreSQL users will be identified using peer authentication. This authenticates the Unix user with the
          same name only, and that without the need for a password.
          This option will never delete existing users or remove permissions, especially not when the value of this
          option is changed. This means that users created and permissions assigned once through this option or
          otherwise have to be removed manually.
        '';
        example = lib.literalExample ''
          [
            {
              name = "nextcloud";
              ensurePermissions = {
                "DATABASE nextcloud" = "ALL PRIVILEGES";
              };
            }
            {
              name = "superuser";
              ensurePermissions = {
                "ALL TABLES IN SCHEMA public" = "ALL PRIVILEGES";
              };
            }
          ]
        '';
      };

      enableTCPIP = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Whether PostgreSQL should listen on all network interfaces.
          If disabled, the database can only be accessed via its Unix
          domain socket or via TCP connections to localhost.
        '';
      };

      logLinePrefix = lib.mkOption {
        type = lib.types.str;
        default = "Postgres [%p] ";
        example = "%m [%p] ";
        description = ''
          A printf-style string that is output at the beginning of each log line.
          Upstream default is <literal>'%m [%p] '</literal>, i.e. it includes the timestamp. We do
          not include the timestamp, because journal has it anyway.
        '';
      };

      extraPlugins = lib.mkOption {
        type = with lib.types; listOf path;
        default = [ ];
        example = lib.literalExample "with pkgs.postgresql_11.pkgs; [ postgis pg_repack ]";
        description = ''
          List of PostgreSQL plugins. PostgreSQL version for each plugin should
          match version for <literal>services.postgresql.package</literal> value.
        '';
      };

      config = lib.mkOption {
        type =
          with lib.types;
          attrsOf (oneOf [
            bool
            float
            int
            str
          ]);
        default = { };
        description = ''
          PostgreSQL configuration. Refer to
          <link xlink:href="https://www.postgresql.org/docs/11/config-setting.html#CONFIG-SETTING-CONFIGURATION-FILE"/>
          for an overview of <literal>postgresql.conf</literal>.
          <note><para>
            String values will automatically be enclosed in single quotes. Single quotes will be
            escaped with two single quotes as described by the upstream documentation linked above.
          </para></note>
        '';
        example = lib.literalExample ''
          {
            log_connections = true;
            log_statement = "all";
            logging_collector = true
            log_disconnections = true
            log_destination = lib.mkForce "syslog";
          }
        '';
      };

      recoveryConfig = lib.mkOption {
        type = with lib.types; nullOr lines;
        default = null;
        description = ''
          Contents of the <filename>recovery.conf</filename> file.
        '';
      };

      superUser = lib.mkOption {
        type = lib.types.str;
        default = "postgres";
        internal = true;
        readOnly = true;
        description = ''
          PostgreSQL superuser account to use for various operations. Internal since changing
          this value would lead to breakage while setting up databases.
        '';
      };
    };
  };
  # END Copyright (c) 2003-2021 Eelco Dolstra and the Nixpkgs/NixOS contributors

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    # BEGIN Copyright (c) 2003-2021 Eelco Dolstra and the Nixpkgs/NixOS contributors
    services.postgresql.config = {
      hba_file = "${pkgs.writeText "pg_hba.conf" cfg.authentication}";
      ident_file = "${pkgs.writeText "pg_ident.conf" cfg.identMap}";
      log_destination = "stderr";
      log_line_prefix = cfg.logLinePrefix;
      listen_addresses = if cfg.enableTCPIP then "*" else "localhost";
      port = cfg.port;
    };

    services.postgresql.dataDir = lib.mkDefault "/var/lib/postgresql/${cfg.package.psqlSchema}";

    services.postgresql.authentication = lib.mkAfter ''
      # Generated file; do not edit!
      local all all              peer
      host  all all 127.0.0.1/32 md5
      host  all all ::1/128      md5
    '';

    users.users.postgres = {
      uid = config.ids.uids.postgres;
      group = "postgres";
      description = "PostgreSQL server user";
      createHome = false;
      home = "${cfg.dataDir}";
      useDefaultShell = true;
    };

    users.groups.postgres.gid = config.ids.gids.postgres;

    # TODO Why?
    # environment.pathsToLink = [
    #  "/share/postgresql"
    # ];

    # TODO Reimplement
    # system.extraDependencies = lib.optional (cfg.checkConfig && pkgs.stdenv.hostPlatform == pkgs.stdenv.buildPlatform) configFileCheck;
    # END Copyright (c) 2003-2021 Eelco Dolstra and the Nixpkgs/NixOS contributors

    init.services.postgresql = {
      environment.PGDATA = cfg.dataDir;

      ensureSomething.create."dataDir" = {
        type = "directory";
        mode = "750";
        owner = "postgres:postgres";
        persistent = true;
        dst = cfg.dataDir;
      };

      ensureSomething.create."runSocket" = {
        type = "directory";
        mode = "755";
        owner = "postgres:postgres";
        persistent = false;
        dst = "/run/postgresql/";
      };

      # BEGIN Copyright (c) 2003-2021 Eelco Dolstra and the Nixpkgs/NixOS contributors
      script = pkgs.writeShellScript "postgresql" ''
        function _sigterm() {
          if ! [ -z "$postgresql" ] && kill -0 "$postgresql" ; then
            kill -TERM "$postgresql"
            wait "$postgresql"
          fi
          exit 0
        }
        trap _sigterm TERM

        if [[ ! -e ${cfg.dataDir}/PG_VERSION ]] ; then
           # Clean up the data directory
           rm -f ${cfg.dataDir}/*.conf

           # Initialize the database
           chpst -u postgres:postgres ${cfg.package}/bin/initdb -U ${cfg.superUser} ${lib.concatStringsSep " " cfg.initdbArgs}

           touch ${cfg.dataDir}/.first_startup
        fi

        ln -sfn ${configFile}/postgresql.conf ${cfg.dataDir}/postgresql.conf
        ${lib.optionalString (cfg.recoveryConfig != null) ''
          ln -sfn "${pkgs.writeText "recovery.conf" cfg.recoveryConfig}" \
          "${cfg.dataDir}/recovery.conf"
        ''}

        chpst -u postgres:postgres ${cfg.package}/bin/postgres &
        postgresql=$!

        PSQL="chpst -u postgres:postgres ${cfg.package}/bin/psql --port=${cfg.port} -X"
        while ! $PSQL -d postgres -c "" 2> /dev/null ; do
          if ! kill -0 "$postgresql"; then exit 1; fi
          sleep 0.1
        done

        $PSQL -f "${pkgs.writeText "postgres-start-early.sql" cfg.earlyStartScript}"

        ${lib.concatMapStrings
          (
            { database, options }:
            ''
              $PSQL -tAc "SELECT 1 FROM pg_database WHERE datname = '${database}'" | grep -q 1 || $PSQL -tAc 'CREATE DATABASE "${database}" ${
                if options != "" then "WITH " + options else ""
              }'
            ''
          )
          (
            (
              if lib.isList cfg.ensureDatabases then
                map (x: {
                  database = x;
                  options = "";
                })
              else
                lib.mapAttrsToList (
                  k: v: {
                    database = k;
                    options = lib.concatStringsSep " " (lib.mapAttrsToList (k: v: "${k} = \"${v}\"") v);
                  }
                )
            )
              cfg.ensureDatabases
          )
        }

        ${lib.concatMapStrings (user: ''
          $PSQL -tAc "SELECT 1 FROM pg_roles WHERE rolname='${user.name}'" | grep -q 1 || $PSQL -tAc 'CREATE USER "${user.name}"'
          ${lib.concatStringsSep "\n" (
            lib.mapAttrsToList (database: permission: ''
              $PSQL -tAc 'GRANT ${permission} ON ${database} TO "${user.name}"'
            '') user.ensurePermissions
          )}
          ${lib.optionalString user.ensureDBOwnership ''$PSQL -tAc 'ALTER DATABASE "${user.name}" OWNER TO "${user.name}";' ''}
        '') cfg.ensureUsers}

        ${lib.concatStrings (
          lib.mapAttrsToList (
            extension: schemas:
            lib.concatMapStrings (schema: ''
              $PSQL -tAc "create extension if not exists ${extension}" ${schema}
            '') schemas
          ) cfg.ensureExtensions
        )}

        if test -e "${cfg.dataDir}/.first_startup"; then
          ${lib.optionalString (cfg.initialScript != null) ''
            $PSQL -f "${cfg.initialScript}" -d postgres
          ''}
          rm -f "${cfg.dataDir}/.first_startup"
        fi

        $PSQL -f "${pkgs.writeText "postgres-start.sql" cfg.startScript}"

        wait $postgresql
      '';
      # END Copyright (c) 2003-2021 Eelco Dolstra and the Nixpkgs/NixOS contributors

      enabled = true;
    };
  };
}
