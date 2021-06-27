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

/* authors:
  Ethan Carter Edwards <ethan@ethancedwards.com>
*/

{ pkgs, config, lib, nglib, ... }:
with lib;
let
# ========================================================================
# -- BEGIN MIT LICENSED CODE
# ========================================================================
  # For the license please refer to COPYING.NIXOS-MIT
  eachSite = config.services.wordpress;
  ids = config.ids;

  defaultUser = "wordpress";

  # allows for multiple installs
  stateDir = hostName: "/var/lib/wordpress/${hostName}";

  pkg = hostName: cfg: pkgs.stdenv.mkDerivation rec {
    pname = "wordpress-${hostName}";
    version = src.version;
    src = cfg.package;

    installPhase = ''
      mkdir -p $out
      cp -r * $out/

      # symlink the wordpress config
      ln -s ${wpConfig hostName cfg} $out/share/wordpress/wp-config.php
      # symlink uploads directory
      ln -s ${cfg.uploadsDir} $out/share/wordpress/wp-content/uploads

      # https://github.com/NixOS/nixpkgs/pull/53399
      #
      # Symlinking works for most plugins and themes, but Avada, for instance, fails to
      # understand the symlink, causing its file path stripping to fail. This results in
      # requests that look like: https://example.com/wp-content//nix/store/...plugin/path/some-file.js
      # Since hard linking directories is not allowed, copying is the next best thing.

      # copy additional plugin(s) and theme(s)
      ${concatMapStringsSep "\n" (theme: "cp -r ${theme} $out/share/wordpress/wp-content/themes/${theme.name}") cfg.themes}
      ${concatMapStringsSep "\n" (plugin: "cp -r ${plugin} $out/share/wordpress/wp-content/plugins/${plugin.name}") cfg.plugins}
    '';
  };

  wpConfig = hostName: cfg: pkgs.writeText "wp-config-${hostName}.php" ''
    <?php
      define('DB_NAME', '${cfg.database.name}');
      define('DB_HOST', '${cfg.database.host}:${if cfg.database.socket != null then cfg.database.socket else toString cfg.database.port}');
      define('DB_USER', '${cfg.database.user}');
      ${optionalString (cfg.database.passwordFile != null) "define('DB_PASSWORD', file_get_contents('${cfg.database.passwordFile}'));"}
      define('DB_CHARSET', 'utf8');
      $table_prefix  = '${cfg.database.tablePrefix}';

      require_once('${stateDir hostName}/secret-keys.php');

      # wordpress is installed onto a read-only file system
      define('DISALLOW_FILE_EDIT', true);
      define('AUTOMATIC_UPDATER_DISABLED', true);

      ${cfg.extraConfig}

      if ( !defined('ABSPATH') )
        define('ABSPATH', dirname(__FILE__) . '/');

      require_once(ABSPATH . 'wp-settings.php');
    ?>
  '';

  siteOpts = { lib, name, ... }:
    {
      options = {
        package = mkOption {
          type = types.package;
          default = pkgs.wordpress;
          description = "Which WordPress package to use.";
        };

        uploadsDir = mkOption {
          type = types.path;
          default = "/var/lib/wordpress/${name}/uploads";
          description = ''
            This directory is used for uploads of pictures. The directory passed here is automatically
            created and permissions adjusted as required.
          '';
        };

        plugins = mkOption {
          type = types.listOf types.path;
          default = [];
          description = ''
            List of path(s) to respective plugin(s) which are copied from the 'plugins' directory.
            <note><para>These plugins need to be packaged before use, see example.</para></note>
          '';
          example = ''
            # Wordpress plugin 'embed-pdf-viewer' installation example
            embedPdfViewerPlugin = pkgs.stdenv.mkDerivation {
              name = "embed-pdf-viewer-plugin";
              # Download the theme from the wordpress site
              src = pkgs.fetchurl {
                url = "https://downloads.wordpress.org/plugin/embed-pdf-viewer.2.0.3.zip";
                sha256 = "1rhba5h5fjlhy8p05zf0p14c9iagfh96y91r36ni0rmk6y891lyd";
              };
              # We need unzip to build this package
              nativeBuildInputs = [ pkgs.unzip ];
              # Installing simply means copying all files to the output directory
              installPhase = "mkdir -p $out; cp -R * $out/";
            };

            And then pass this theme to the themes list like this:
              plugins = [ embedPdfViewerPlugin ];
          '';
        };

        themes = mkOption {
          type = types.listOf types.path;
          default = [];
          description = ''
            List of path(s) to respective theme(s) which are copied from the 'theme' directory.
            <note><para>These themes need to be packaged before use, see example.</para></note>
          '';
          example = ''
            # Let's package the responsive theme
            responsiveTheme = pkgs.stdenv.mkDerivation {
              name = "responsive-theme";
              # Download the theme from the wordpress site
              src = pkgs.fetchurl {
                url = "https://downloads.wordpress.org/theme/responsive.3.14.zip";
                sha256 = "0rjwm811f4aa4q43r77zxlpklyb85q08f9c8ns2akcarrvj5ydx3";
              };
              # We need unzip to build this package
              nativeBuildInputs = [ pkgs.unzip ];
              # Installing simply means copying all files to the output directory
              installPhase = "mkdir -p $out; cp -R * $out/";
            };

            And then pass this theme to the themes list like this:
              themes = [ responsiveTheme ];
          '';
        };

        database = {
          host = mkOption {
            type = types.str;
            default = "localhost";
            description = "Database host address.";
          };

          port = mkOption {
            type = types.port;
            default = 3306;
            description = "Database host port.";
          };

          name = mkOption {
            type = types.str;
            default = "wordpress";
            description = "Database name.";
          };

          user = mkOption {
            type = types.str;
            default = "wordpress";
            description = "Database user.";
          };

          passwordFile = mkOption {
            type = types.nullOr types.path;
            default = null;
            example = "/run/keys/wordpress-dbpassword";
            description = ''
              A file containing the password corresponding to
              <option>database.user</option>.
            '';
          };

          tablePrefix = mkOption {
            type = types.str;
            default = "wp_";
            description = ''
              The $table_prefix is the value placed in the front of your database tables.
              Change the value if you want to use something other than wp_ for your database
              prefix. Typically this is changed if you are installing multiple WordPress blogs
              in the same database.

              See <link xlink:href='https://codex.wordpress.org/Editing_wp-config.php#table_prefix'/>.
            '';
          };

          socket = mkOption {
            type = types.nullOr types.path;
            default = null;
            defaultText = "/run/mysqld/mysqld.sock";
            description = "Path to the unix socket file to use for authentication.";
          };

          createLocally = mkOption {
            type = types.bool;
            default = true;
            description = "Create the database and database user locally.";
          };
        };

        virtualHost = mkOption {
          type = types.submodule (import ../web-servers/apache-httpd/vhost-options.nix);
          example = literalExample ''
            {
              adminAddr = "webmaster@example.org";
              forceSSL = true;
              enableACME = true;
            }
          '';
          description = ''
            Apache configuration can be done by adapting <option>services.httpd.virtualHosts</option>.
          '';
        };

        poolConfig = mkOption {
          type = with types; attrsOf (oneOf [ str int bool ]);
          default = {
            "pm" = "dynamic";
            "pm.max_children" = 32;
            "pm.start_servers" = 2;
            "pm.min_spare_servers" = 2;
            "pm.max_spare_servers" = 4;
            "pm.max_requests" = 500;
          };
          description = ''
            Options for the WordPress PHP pool. See the documentation on <literal>php-fpm.conf</literal>
            for details on configuration directives.
          '';
        };

        extraConfig = mkOption {
          type = types.lines;
          default = "";
          description = ''
            Any additional text to be appended to the wp-config.php
            configuration file. This is a PHP script. For configuration
            settings, see <link xlink:href='https://codex.wordpress.org/Editing_wp-config.php'/>.
          '';
          example = ''
            define( 'AUTOSAVE_INTERVAL', 60 ); // Seconds
          '';
        };
      };

      config.virtualHost.hostName = mkDefault name;
    };
# =======================================================================
# -- END MIT LICENSED CODE
# ========================================================================
in
{
  options = {
    # -- BEGIN MIT LICENSED CODE
    services.wordpress = mkOption {
      type = types.attrOf (types.submodule siteOpts);
      default = {};
      description = "Specification of one or more WordPress sites to serve via Apache.";
    };
    # -- END MIT LICENSED CODE
  };
}
