# SPDX-FileCopyrightText:  2023 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ nglib, lib, config, ... }:
{
  options.networking = {
    hostName = lib.mkOption {
      default = null;
      type = lib.types.nullOr (lib.types.strMatching
        "^$|^[[:alnum:]]([[:alnum:]_-]{0,61}[[:alnum:]])?$");
      description = ''
        The hostname of the system. Notice that in many container engines
        the hostname cannot be altered.
      '';
    };
  };

  config.system.activation.hostname = nglib.dag.dagEntryAnywhere (lib.optionalString (config.networking.hostName != null) ''
    echo "${config.networking.hostName}" > /proc/sys/kernel/hostname
  '');
}
