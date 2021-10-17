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
  *
  * This file incorporates work covered by the following copyright and
  * permission notice:
  *
  *     Copyright (c) 2003-2021 Eelco Dolstra and the Nixpkgs/NixOS contributors
  *
  *     Permission is hereby granted, free of charge, to any person obtaining
  *     a copy of this software and associated documentation files (the
  *     "Software"), to deal in the Software without restriction, including
  *     without limitation the rights to use, copy, modify, merge, publish,
  *     distribute, sublicense, and/or sell copies of the Software, and to
  *     permit persons to whom the Software is furnished to do so, subject to
  *     the following conditions:
  *
  *     The above copyright notice and this permission notice shall be
  *     included in all copies or substantial portions of the Software.
  *
  *     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  *     EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  *     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  *     NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
  *     LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
  *     OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
  *     WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.services.mysql;

  isMariaDB = lib.getName cfg.package == lib.getName pkgs.mariadb;

  mysqldOptions =
    "--user=${cfg.user} --datadir=${cfg.dataDir} --basedir=${cfg.package}";

  configFile = pkgs.writeText "my.cnf" (
    generators.toINI { listsAsDuplicateKeys = true; } cfg.config
  );
in
{
  options.services.mysql =
    {
      enable = mkEnableOption "MySQL Server";

      package = mkOption {
        type = types.package;
        example = literalExample "pkgs.mariadb";
        description = ''
          MySQL package to use. You can also use MariaDB and this module will re-adjust.
        '';
        default = pkgs.mariadb;
      };

      port = mkOption {
        type = types.int;
        default = 3306;
        description = ''
          The port on which MariaDB listens.
        '';
        apply = toString;
      };

      bind = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = literalExample "127.0.0.1";
        description = ''
          Address to bind to.
        '';
      };

      user = mkOption {
        type = types.str;
        default = "mysql";
        description = "User account under which MySQL runs.";
      };

      group = mkOption {
        type = types.str;
        default = "mysql";
        description = "Group under which MySQL runs.";
      };

      dataDir = mkOption {
        type = types.path;
        default = "/var/lib/mysql";
        description = "Location where MySQL stores its table files.";
      };

      config = mkOption {
        type = with types; attrsOf (attrsOf (oneOf [ bool int str (listOf str) ]));
        default = { };
        example = literalExample
          ''
            {
              mysqld = {
                key_buffer_size = "6G";
                table_cache = 1600;
                log-error = "/var/log/mysql_err.log";
                plugin-load-add = [ "server_audit" "ed25519=auth_ed25519" ];
              };
              mysqldump = {
                quick = true;
                max_allowed_packet = "16M";
              };
            }
          '';
      };

      initialScript = mkOption {
        type = with types; nullOr (oneOf [ package str ]);
        default = null;
        description = ''
          A file containing SQL statements to execute on first startup.
        '';
      };

      ensureDatabases = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = ''
          Ensures that the specified databases exist.
          This option will never delete existing databases, especially not when the value of this
          option is changed. This means that databases created once through this option or
          otherwise have to be removed manually.
        '';
        example = [
          "gitea"
          "nextcloud"
        ];
      };

      ensureUsers = mkOption {
        type = types.listOf (types.submodule {
          options = {
            name = mkOption {
              type = types.str;
              description = ''
                Name of the user to ensure.
              '';
            };
            ensurePermissions = mkOption {
              type = types.attrsOf types.str;
              default = { };
              description = ''
                Permissions to ensure for the user, specified as an attribute set.
                The attribute names specify the database and tables to grant the permissions for.
                The attribute values specify the permissions to grant. You may specify one or
                multiple comma-separated SQL privileges here.
                For more information on how to specify the target
                and on which privileges exist, see the
                <link xlink:href="https://mariadb.com/kb/en/library/grant/">GRANT syntax</link>.
                The attributes are used as <code>GRANT ''${attrName} ON ''${attrValue}</code>.
              '';
              example = literalExample ''
                {
                  "database.*" = "ALL PRIVILEGES";
                  "*.*" = "SELECT, LOCK TABLES";
                }
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
        example = literalExample ''
          [
            {
              name = "nextcloud";
              ensurePermissions = {
                "database.*" = "ALL PRIVILEGES";
              };
            }
            {
              name = "superuser";
              ensurePermissions = {
                "*.*" = "SELECT, LOCK TABLES";
              };
            }
          ]
        '';
      };
    };

  config = mkIf cfg.enable
    {
      services.mysql.config.mysqld = {
        datadir = cfg.dataDir;
        bind-address = mkIf (cfg.bind != null) cfg.bind;
        port = cfg.port;
      };

      users.users.mysql = mapAttrs (_: mkDefault)
        {
          uid = config.ids.uids.mysql;
          group = "mysql";
          description = "MySQL server user";
          createHome = false;
          home = "${cfg.dataDir}";
          useDefaultShell = true;
        };

      users.groups.mysql.gid = config.ids.gids.mysql;

      init.services.mysql = {
        ensureSomething.create."dataDir" = {
          type = "directory";
          mode = "750";
          owner = "${cfg.user}:${cfg.group}";
          persistent = true;
          dst = cfg.dataDir;
        };

        ensureSomething.create."runSocket" = {
          type = "directory";
          mode = "755";
          owner = "${cfg.user}:${cfg.group}";
          persistent = false;
          dst = "/run/mysqld/";
        };

        script = pkgs.writeShellScript "mysql" ''
          if [[ ! -e ${cfg.dataDir}/mysql ]] ; then
            test -e ${cfg.package}/bin/mysql_install_db && ${cfg.package}/bin/mysql_install_db --defaults-file=${configFile} ${mysqldOptions}
            test -e ${cfg.package}/bin/mysqld && ${cfg.package}/bin/mysqld --defaults-file=${configFile} ${mysqldOptions} --initialize-insecure

            touch ${cfg.dataDir}/.first_startup
          fi

          chpst -u ${cfg.user}:${cfg.group} ${cfg.package}/bin/mysqld --defaults-file=${configFile} ${mysqldOptions} &
          mysql=$!

          while ! [[ -e /run/mysqld/mysqld.sock ]] ; do
            if ! kill -0 "$mysql"; then exit 1; fi
            sleep 0.1
          done

          if [[ -e ${cfg.dataDir}/.first_startup ]] ; then
            # While MariaDB comes with a 'mysql' super user account since 10.4.x, MySQL does not
            # Since we don't want to run this service as 'root' we need to ensure the account exists on first run
            ( echo "CREATE USER IF NOT EXISTS '${cfg.user}'@'localhost' IDENTIFIED WITH ${if isMariaDB then "unix_socket" else "auth_socket"};"
              echo "GRANT ALL PRIVILEGES ON *.* TO '${cfg.user}'@'localhost' WITH GRANT OPTION;"
            ) | ${cfg.package}/bin/mysql -u root -N

            ${optionalString (cfg.initialScript != null)
              ''
                # Execute initial script
                # using toString to avoid copying the file to nix store if given as path instead of string,
                # as it might contain credentials
                cat ${toString cfg.initialScript} | ${cfg.package}/bin/mysql -u ${cfg.user} -N
              ''}

              rm ${cfg.dataDir}/.first_startup
          fi

          ${optionalString (cfg.ensureDatabases != []) ''
            (
            ${concatMapStrings (database: ''
              echo "CREATE DATABASE IF NOT EXISTS \`${database}\`;"
            '') cfg.ensureDatabases}
            ) | ${cfg.package}/bin/mysql -N
          ''}
          ${concatMapStrings (user:
            ''
              ( echo "CREATE USER IF NOT EXISTS '${user.name}'@'localhost' IDENTIFIED WITH ${if isMariaDB then "unix_socket" else "auth_socket"};"
                ${concatStringsSep "\n" (mapAttrsToList (database: permission: ''
                  echo "GRANT ${permission} ON ${database} TO '${user.name}'@'localhost';"
                '') user.ensurePermissions)}
              ) | ${cfg.package}/bin/mysql -N
            '') cfg.ensureUsers}

          wait $mysql
        '';

        enabled = true;
      };
    };
}
