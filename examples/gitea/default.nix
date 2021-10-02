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

nglib:
((nglib "x86_64-linux").makeSystem {
  system = "x86_64-linux";
  name = "nixng-gitea";
  config = ({ pkgs, ... }:
    {
      dumb-init = {
        enable = true;
        type.services = { };
      };
      init.services.gitea.shutdownOnExit = true;
      services.gitea = {
        enable = true;

        appName = "Gitea";
        runMode = "prod";
        user = "gitea";

        secrets = {
          secretKeyFile = "/secret_key";
          internalTokenFile = "/internal_token";
          jwtSecretFile = "/jwt_secret";
          lfsJwtSecretFile = "/lfs_jwt_secret";
        };

        configuration = {
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
            ROOT_URL = http://localhost:3000/;
            DISABLE_SSH = false;
            SSH_PORT = 22;
            SSH_LISTEN_PORT = 22;
            LFS_START_SERVER = true;
            LFS_CONTENT_PATH = "/data/gitea/git/lfs";
            DOMAIN = "localhost";
            LFS_JWT_SECRET = "#lfsJwtSecret#";
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
            SECRET_KEY = "#secretKey";
            INTERNAL_TOKEN = "#internalToken#";
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

          oauth2.JWT_SECRET = "#jwtSecret#";

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
})
