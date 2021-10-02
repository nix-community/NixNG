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
(nglib "x86_64-linux").makeSystem {
  system = "x86_64-linux";
  name = "nixng-pantalaimon";
  config = ({ pkgs, ... }:
    {
      dumb-init = {
        enable = true;
        type.services = { };
      };
      init.services.pantalaimon = {
        shutdownOnExit = true;
      };

      services.pantalaimon = {
        enable = true;

        package = (pkgs.pantalaimon.override
          { enableDbusUi = false; }).overrideAttrs (old: {
          version = "0.10.2";
          src = pkgs.fetchFromGitHub {
            owner = "matrix-org";
            repo = "pantalaimon";
            rev = "0.10.2";
            sha256 = "sha256-sjaJomKMKSZqLlKWTG7Oa87dXa5SnGQlVnrdS707A1w=";
          };
          patches = [ ];
        });

        config = {
          Default =
            {
              LogLevel = "Debug";
              SSL = "True";
              Notifications = "Off";
            };

          Clockwork =
            {
              Homeserver = "https://matrix.org";
              ListenAddress = "0.0.0.0";
              ListenPort = 80;
              SSL = "False";
            };
        };
      };
    });
}
