# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-postfix";
  config = ({ pkgs, config, ... }:
    let
      pgsql = cfg:
        "pgsql:" +
        pkgs.writeText "pgsql-virtual-mailbox-domains.cf"
          (nglib.generators.postfix.toMainCnf cfg);
      dovecotExt = name: cfg:
        pkgs.writeText (name + ".conf.ext")
          (nglib.generators.toDovecot cfg);
    in
    {
      config = {
        dumb-init = {
          enable = true;
          type.services = { };
        };
        init.services.postfix = {
          shutdownOnExit = true;

          ensureSomething.create."mailDir" = {
            type = "directory";
            mode = "755";
            owner = "5000:5000";
            persistent = true;
            dst = "/var/mail/vhosts";
          };

          ensureSomething.create."postfixSpoolDir" = {
            type = "directory";
            mode = "750";
            owner = "root:root";
            persistent = false;
            dst = "/var/spool/postfix/";
          };
        };
        init.services.dovecot = {
          shutdownOnExit = true;

          ensureSomething.create."dovecotSockets" = {
            type = "directory";
            mode = "755";
            owner = "postgres:postgres";
            persistent = false;
            dst = "/var/spool/postfix/private/";
          };
        };
        init.services.postgresql = {
          shutdownOnExit = true;

          ensureSomething.create."postfixRunSocket" = {
            type = "directory";
            mode = "755";
            owner = "postgres:postgres";
            persistent = false;
            dst = "/var/spool/postfix/run/postgresql/";
          };
        };

        services.postgresql = {
          enable = true;
          package = pkgs.postgresql_12;

          config = {
            unix_socket_directories =
              "/run/postgresql/, /var/spool/postfix/run/postgresql/";
          };

          initialScript = pkgs.writeText "init.sql" ''
            \c mailserver;

            CREATE TABLE IF NOT EXISTS virtual_domains (
              id   SERIAL         NOT NULL,
              name VARCHAR ( 50 ) NOT NULL,

              PRIMARY KEY (id)
            );

            CREATE TABLE IF NOT EXISTS virtual_users (
              id        SERIAL          NOT NULL,
              domain_id INT             NOT NULL,
              password  VARCHAR ( 106 ) NOT NULL,
              email     VARCHAR ( 100 ) NOT NULL UNIQUE,
              quota     VARCHAR ( 20 )  DEFAULT '500M',

              PRIMARY KEY ( id ),
              FOREIGN KEY ( domain_id ) REFERENCES virtual_domains( id ) ON DELETE CASCADE
            );

            CREATE TABLE IF NOT EXISTS virtual_aliases (
              id          SERIAL          NOT NULL,
              domain_id   INT             NOT NULL,
              source      VARCHAR ( 100 ) NOT NULL,
              destination VARCHAR ( 100 ) NOT NULL,
              PRIMARY KEY ( id ),
              FOREIGN KEY ( domain_id ) REFERENCES virtual_domains( id ) ON DELETE CASCADE
            );

            GRANT SELECT ON ALL TABLES IN SCHEMA public TO dovecot;
            ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO dovecot;

            GRANT SELECT ON ALL TABLES IN SCHEMA public TO postfix;
            ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO postfix;

            INSERT INTO public.virtual_domains (name) VALUES ('example.org');
            INSERT INTO public.virtual_users
              (domain_id,email,password)
              VALUES
              ('1','webmaster@example.org','[password-hash]');
          '';
          ensureDatabases = [ "mailserver" ];
          ensureUsers =
            [{
              name = "dovecot";
              ensurePermissions = { };
            }
              {
                name = "postfix";
                ensurePermissions = { };
              }];
        };

        services.socklog = {
          enable = true;
          unix = "/dev/log";
        };

        services.dovecot = {
          enable = true;
          package = pkgs.dovecot.override
            {
              withSQLite = false;
              withPgSQL = true;
            };

          config = {
            # auth
            disable_plaintext_auth = "yes";
            auth_mechanisms = "plain login";

            protocol."imap" = { };

            service."imap-login" = {
              inet_listener."imap" = {
                port = 0;
              };

              inet_listener."imaps" = {
                port = 993;
                ssl = "yes";
              };
            };

            protocol."lmtp" = { };

            service."lmtp" = {
              unix_listener."/var/spool/postfix/private/dovecot-lmtp" = {
                mode = "0600";
                user = "postfix";
                group = "postfix";
              };
            };

            service."auth" = {
              unix_listener."/var/spool/postfix/private/auth" = {
                mode = "0666";
                user = "postfix";
                group = "postfix";
              };
              unix_listener."auth-userdb" = {
                mode = "0600";
                user = "vmail";
              };
              user = "dovecot";
            };

            passdb."" = {
              driver = "sql";
              args = dovecotExt "dovecot-sql"
                {
                  driver = "pgsql";
                  connect = "host=/run/postgresql dbname=mailserver user=dovecot";
                  default_pass_scheme = "SHA512-CRYPT";
                  password_query =
                    ''
                      SELECT email AS user, password, \
                      'vmail' AS userdb_uid, 'vmail' AS userdb_gid, \
                      CONCAT('/var/mail/vhosts/domain.com/',SPLIT_PART(email,'@',1)) AS userdb_home, \
                      CONCAT('*:storage=', quota) AS userdb_quota_rule \
                      FROM virtual_users WHERE email='%u';
                    '';
                  user_query =
                    ''
                      SELECT CONCAT('/var/mail/vhosts/domain.com/',SPLIT_PART(email,'@',1)) AS home, \
                      'vmail' AS uid, 'vmail' AS gid, CONCAT('*:storage=', quota) AS quota_rule \
                      FROM virtual_users WHERE email='%u';
                      iterate_query = SELECT SPLIT_PART(email,'@',1) AS username, \
                      SPLIT_PART(email,'@',2) AS domain \
                      FROM virtual_user;
                    '';
                };
            };

            userdb."" = {
              driver = "sql";
              args = dovecotExt "dovecot-sql"
                {
                  driver = "pgsql";
                  connect = "host=/run/postgresql dbname=mailserver user=dovecot";
                  default_pass_scheme = "SHA512-CRYPT";
                  password_query =
                    ''
                      SELECT email AS user, password, \
                      'vmail' AS userdb_uid, 'vmail' AS userdb_gid, \
                      CONCAT('/var/mail/vhosts/domain.com/',SPLIT_PART(email,'@',1)) AS userdb_home, \
                      CONCAT('*:storage=', quota) AS userdb_quota_rule \
                      FROM virtual_users WHERE email='%u';
                    '';
                  user_query =
                    ''
                      SELECT CONCAT('/var/mail/vhosts/domain.com/',SPLIT_PART(email,'@',1)) AS home, \
                      'vmail' AS uid, 'vmail' AS gid, CONCAT('*:storage=', quota) AS quota_rule \
                      FROM virtual_users WHERE email='%u';
                      iterate_query = SELECT SPLIT_PART(email,'@',1) AS username, \
                      SPLIT_PART(email,'@',2) AS domain \
                      FROM virtual_user;
                    '';
                };
            };
          };
        };

        services.postfix = {
          package = pkgs.postfix.override
            {
              withPgSQL = true;
              withLDAP = false;
              postgresql = pkgs.postgresql_12;
            };
          enable = true;

          masterConfig = { };

          mainConfig =
            {
              compatibility_level = 3;
              mydomain = "example.org";
              myorigin = "$mydomain";
              myhostname = "mail.example.org";

              alias_maps = "inline:{ root=root }";

              mydestination =
                [
                  "localhost"
                  "mail.example.org"
                ];

              mynetworks_style = "host";
              relay_domains = "";

              virtual_mailbox_domains = pgsql
                {
                  user = "postfix";
                  hosts = "/run/postgresql/";
                  dbname = "mailserver";
                  query = "SELECT 1 FROM virtual_domains WHERE name='%s'";
                };

              virtual_mailbox_maps = pgsql
                {
                  user = "postfix";
                  hosts = "/run/postgresql/";
                  dbname = "mailserver";
                  query = "SELECT 1 FROM virtual_users WHERE email='%s'";
                };

              virtual_alias_maps = pgsql
                {
                  user = "postfix";
                  hosts = "/run/postgresql/";
                  dbname = "mailserver";
                  query = "SELECT destination FROM virtual_aliases WHERE source='%s'";
                };
              virtual_mailbox_base = "/var/mail/vhosts";
              virtual_minimum_uid = 100;
              virtual_uid_maps = "static:5000";
              virtual_gid_maps = "static:5000";

              virtual_transport = "lmtp:unix:/var/spool/postfix/private/dovecot-lmtp";
            };
        };
      };
    });
}
