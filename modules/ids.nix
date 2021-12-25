# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ lib, ... }:
with lib;
{
  options.ids = {
    uids = mkOption {
      description = "A username to uid map, used for keeping track of assigned uids.";
      type = with types; attrsOf int;
    };
    gids = mkOption {
      description = "A groupname to gid map, used for keeping track of assigned gids.";
      type = with types; attrsOf int;
    };
  };

  config = {
    ids.uids = {
      root = 0;
      postfix = 13;
      #postdrop = 14;
      dovecot = 46;
      dovenull = 47;
      www-data = 54;
      nginx = 60;
      postgres = 71;
      mysql = 84;
      hydra = 122;
      hydra-queue-runner = 235;
      hydra-www = 236;
      pantalaimon = 398; # might change!
      gitea = 399; # might change!
      jmusicbot = 400;
      mosquitto = 401;
      zigbee2mqtt = 402;
      home-assistant = 403;
      vmail = 5000;
      nobody = 65534;
    };

    ids.gids = {
      root = 0;
      postfix = 13;
      postdrop = 14;
      dovecot = 46;
      dovenull = 47;
      www-data = 54;
      nginx = 60;
      postgres = 71;
      mysql = 84;
      hydra = 122;
      hydra-queue-runner = 235;
      hydra-www = 236;
      pantalaimon = 398; # might change!
      log = 399;
      jmusicbot = 400;
      mosquitto = 401;
      zigbee2mqtt = 402;
      home-assistant = 403;
      vmail = 5000;
      nogroup = 65534;
    };
  };
}
