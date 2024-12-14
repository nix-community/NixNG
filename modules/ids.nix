# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ lib, ... }:
{
  options.ids = {
    uids = lib.mkOption {
      description = "A username to uid map, used for keeping track of assigned uids.";
      type = with lib.types; attrsOf int;
    };
    gids = lib.mkOption {
      description = "A groupname to gid map, used for keeping track of assigned gids.";
      type = with lib.types; attrsOf int;
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
      syncthing = 404;
      dnsmasq = 405;
      attic = 406;
      ntfy-sh = 407;
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
      syncthing = 404;
      dnsmasq = 405;
      attic = 406;
      ntfy-sh = 407;
      vmail = 5000;
      nogroup = 65534;
    };
  };
}
