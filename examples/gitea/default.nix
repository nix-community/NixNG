nglib:
((nglib "x86_64-linux").makeSystem {
  system = "x86_64-linux";
  name = "nixng-gitea";
  config = ({ pkgs, ... }:
    {
      dumb-init = {
        enable = true;
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
            APP_DATA_PATH    = "/data/gitea";
            SSH_DOMAIN       = "localhost";
            HTTP_PORT        = 3000;
            ROOT_URL         = http://localhost:3000/;
            DISABLE_SSH      = false;
            SSH_PORT         = 22;
            SSH_LISTEN_PORT  = 22;
            LFS_START_SERVER = true;
            LFS_CONTENT_PATH = "/data/gitea/git/lfs";
            DOMAIN           = "localhost";
            LFS_JWT_SECRET   = "#lfsJwtSecret#";
            OFFLINE_MODE     = false;
          };

          database = {
            PATH     = "/data/gitea/db.sqlite";
            DB_TYPE  = "sqlite3";
            CHARSET  = "utf8";
          };

          indexer = {
            ISSUE_INDEXER_PATH = "/data/gitea/gitea/indexers/issues.bleve";
            REPO_INDEXER_PATH = "/data/gitea/gitea/indexers/repos.bleve";
          };
          session = {
            PROVIDER_CONFIG = "/data/gitea/gitea/sessions";
            PROVIDER        = "file";
          };

          picture = {
            AVATAR_UPLOAD_PATH            = "/data/gitea/gitea/avatars";
            REPOSITORY_AVATAR_UPLOAD_PATH = "/data/gitea/gitea/repo-avatars";
            DISABLE_GRAVATAR              = false;
            ENABLE_FEDERATED_AVATAR       = true;
          };

          attachment = {
            PATH = "/data/gitea/gitea/attachments";
          };

          security = {
            INSTALL_LOCK   = true;
            SECRET_KEY     = "#secretKey";
            INTERNAL_TOKEN = "#internalToken#";
          };

          service = {
            DISABLE_REGISTRATION              = false;
            REQUIRE_SIGNIN_VIEW               = false;
            REGISTER_EMAIL_CONFIRM            = false;
            ENABLE_NOTIFY_MAIL                = false;
            ALLOW_ONLY_EXTERNAL_REGISTRATION  = false;
            ENABLE_CAPTCHA                    = false;
            DEFAULT_KEEP_EMAIL_PRIVATE        = false;
            DEFAULT_ALLOW_CREATE_ORGANIZATION = true;
            DEFAULT_ENABLE_TIMETRACKING       = true;
            NO_REPLY_ADDRESS                  = "noreply.localhost";
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
