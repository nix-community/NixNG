# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{ lib, ... }:
{
  imports = [
    (lib.mkRemovedOptionModule [
      "services"
      "minecraft"
    ] "The Minecraft Forge module was removed due to being completely broken.")
  ];
}
