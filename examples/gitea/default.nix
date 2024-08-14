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
  name = "nixng-gitea";
  config = ({ lib, config, ... }:
    {
      dumb-init = {
        enable = true;
        type.services = { };
      };
      init.services.gitea.shutdownOnExit = true;
      services.gitea = {
        enable = true;

        secrets = {
          secretKey = {
            source.file = "/secret_key";
            generate = ''
              _target="$1"
              ${lib.getExe config.services.gitea.package} generate secret SECRET_KEY > $_target
            '';
          };
          internalToken = {
            source.file = "/internal_token";
            generate = ''
              _target="$1"
              ${lib.getExe config.services.gitea.package} generate secret INTERNAL_TOKEN > $_target
            '';
          };
          jwtSecret = {
            source.file = "/jwt_secret";
            generate = ''
              _target="$1"
              ${lib.getExe config.services.gitea.package} generate secret JWT_SECRET > $_target
            '';
          };
          lfsJwtSecret = {
            source.file = "/lfs_jwt_secret";
            generate = ''
              _target="$1"
              ${lib.getExe config.services.gitea.package} generate secret LFS_JWT_SECRET > $_target
            '';
          };
        };

        settings = {
          default = {
            APP_NAME = "Gitea";
            RUN_MODE = "prod";
            RUN_USER = "gitea";
          };

          repository = {
            ROOT = "/data/gitea/git/repositories";
          };

          "repository.local" = {
            LOCAL_COPY_PATH = "/data/gitea/tmp/local-repo";
          };

          "repository.upload" = {
            TEMP_PATH = "/data/gitea/gitea/uploads";
          };

          server = {
            APP_DATA_PATH = "/data/gitea";
            SSH_DOMAIN = "localhost";
            HTTP_PORT = 3000;
            ROOT_URL = "http://localhost:3000/";
            DISABLE_SSH = false;
            SSH_PORT = 22;
            SSH_LISTEN_PORT = 22;
            LFS_START_SERVER = true;
            LFS_CONTENT_PATH = "/data/gitea/git/lfs";
            DOMAIN = "localhost";
            LFS_JWT_SECRET = "@lfsJwtSecret@";
            OFFLINE_MODE = false;
          };

          database = {
            PATH = "/data/gitea/db.sqlite";
            DB_TYPE = "sqlite3";
            CHARSET = "utf8";
          };

          indexer = {
            ISSUE_INDEXER_PATH = "/data/gitea/gitea/indexers/issues.bleve";
            REPO_INDEXER_PATH = "/data/gitea/gitea/indexers/repos.bleve";
          };
          session = {
            PROVIDER_CONFIG = "/data/gitea/gitea/sessions";
            PROVIDER = "file";
          };

          picture = {
            AVATAR_UPLOAD_PATH = "/data/gitea/gitea/avatars";
            REPOSITORY_AVATAR_UPLOAD_PATH = "/data/gitea/gitea/repo-avatars";
            DISABLE_GRAVATAR = false;
            ENABLE_FEDERATED_AVATAR = true;
          };

          attachment = {
            PATH = "/data/gitea/gitea/attachments";
          };

          security = {
            INSTALL_LOCK = true;
            SECRET_KEY = "@secretKey@";
            INTERNAL_TOKEN = "@internalToken@";
          };

          service = {
            DISABLE_REGISTRATION = false;
            REQUIRE_SIGNIN_VIEW = false;
            REGISTER_EMAIL_CONFIRM = false;
            ENABLE_NOTIFY_MAIL = false;
            ALLOW_ONLY_EXTERNAL_REGISTRATION = false;
            ENABLE_CAPTCHA = false;
            DEFAULT_KEEP_EMAIL_PRIVATE = false;
            DEFAULT_ALLOW_CREATE_ORGANIZATION = true;
            DEFAULT_ENABLE_TIMETRACKING = true;
            NO_REPLY_ADDRESS = "noreply.localhost";
          };

          oauth2.JWT_SECRET = "@jwtSecret@";

          mailer.ENABLED = false;

          openid = {
            ENABLE_OPENID_SIGNIN = true;
            ENABLE_OPENID_SIGNUP = true;
          };

          log = {
            MODE = "console";
            LEVEL = "Debug";
          };
        };
      };
    }
  );
}
