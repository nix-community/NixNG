# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

# http://www.linuxfromscratch.org/blfs/view/svn/postlfs/initramfs.html

export PATH=$PATH:@bash@/bin:@busybox@/bin:@eudev@/bin

echo "Hello"

problem()
{
   printf "Encountered a problem!\n\nDropping you to a shell.\n\n"
   sh
}

do_mount_root() {
    mkdir /.root
    [ -n "$rootflags" ] && rootflags="$rootflags,"
    rootflags="$rootflags$ro"

    case "$root" in
	/dev/*    ) device=$root ;;
	UUID=*    ) eval $root; device="/dev/disk/by-uuid/$UUID" ;;
	PARTUUID=*) eval $root; device="/dev/disk/by-partuuid/$PARTUUID" ;;
	LABEL=*   ) eval $root; device="/dev/disk/by-label/$LABEL" ;;
	""        ) echo "No root device specified." ; problem ;;
    esac

    while [ ! -b "$device" ] ; do
	no_device $device
	problem
    done

    if ! mount -n -t "$rootfstype" -o "$rootflags" "$device" /.root ; then
	no_mount $device
	cat /proc/partitions
	while true ; do sleep 10000 ; done
    else
	echo "Successfully mounted device $root"
    fi
}

echo "Stage 1 commence"

mkdir -p /dev /proc /sys /run
mount -n -t devtmpfs devtmpfs /dev
mount -n -t proc     proc     /proc
mount -n -t sysfs    sysfs    /sys
mount -n -t tmpfs    tmpfs    /run

read -r cmdline < /proc/cmdline

for param in $cmdline ; do
    case $param in
	init=*) init=${param#init=} ;;
	root=*) root=${param#root=} ;;
	rootdelay=*) rootdelay=${param#rootdelay=} ;;
	rootfstype=*) rootfstype=${param#rootfstype=} ;;
	rootflags=*) rootflags=${param#rootflags=} ;;
	noresume=*) noresume=true ;;
	ro=*) ro="ro" ;;
	rw=*) ro="rw" ;;
    esac
done

udevd --daemon --resolve-names=never
udevadm trigger
udevadm settle

if [ -n "$rootdelay"    ] ; then sleep "$rootdelay" ; fi

bash
