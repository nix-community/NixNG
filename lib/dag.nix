# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ lib, ... }:
import (builtins.fetchurl {
  url = "https://raw.githubusercontent.com/nix-community/home-manager/45abf3d38a2b51c00c347cab6950f3734e023bba/modules/lib/dag.nix";
  sha256 = "sha256-NN9iKanf86D1MH9Nx8nsQj9T2+Poy9XeW9pLcZIyFHU=";
}) { inherit lib; }
