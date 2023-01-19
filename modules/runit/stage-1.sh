# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

echo "<--- Stage 2.1 --->"

mkdir -p /proc /sys
mount -t proc none /proc
mount -t sysfs none /sys

mount -t devtmpfs devtmpfs /dev
mkdir -p /dev/shm /dev/pts
mount -t tmpfs -o "mode=1777" none /dev/shm
mount -t devpts none /dev/pts
mkdir -p /tmp
mount -t tmpfs none /tmp

# Run activation script for this system
"$_system_config/activation"

. /etc/profile
