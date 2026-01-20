# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  lib,
  util-linux,
  writeShellScript,
}:
{ n, s }:
writeShellScript "${n}-log" ''
  ${
    if s.log.file != null then
      with s.log.file;
      assert rotate == 0;
      ''
        cat > ${dst}
      ''
    else if s.log.syslog != null then
      with s.log.syslog;
      let
        inetDst = ''-n "$(echo "${dst}" | cut -d : -f 1)" -P "$(echo "${dst}" | cut -d : -f 2)"'';
        connection =
          if type == "uds" then
            "-u ${dst}"
          else if type == "udp" then
            ''-d ${inetDst}''
          else if type == "tcp" then
            ''-T ${inetDst}''
          else
            abort "Unknown log type, module system should have caught this!";
        rfc5424 = optional (!time) "notime" ++ optional (!timeQuality) "notq" ++ optional (!host) "nohost";
      in
      ''
        export PATH=${util-linux}/bin:$PATH
        logger ${connection} \
          ${if rfc5424 == [ ] then "--rfc5424" else "--rfc5424=${concatStringsSep "," rfc5424}"} \
          ${optionalString (tag != null) "-t ${tag}"} \
          ${optionalString (priority != null) "-t ${priority}"} \
      ''
    else
      "cat" # | sed -u 's/^/${n} => /' - busybox sed doesnt support unbuffered
  }
''
