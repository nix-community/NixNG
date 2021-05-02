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

{ lib
, utillinux
, writeShellScript
}:
{ n, s }:
with lib;
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
        inetDst =
          ''-n "$(echo "${dst}" | cut -d : -f 1)" -P "$(echo "${dst}" | cut -d : -f 2)"'';
        connection =
          if type == "uds" then
            "-u ${dst}"
          else if type == "udp" then
            ''-d ${inetDst}''
          else if type == "tcp" then
            ''-T ${inetDst}''
          else
            abort "Unknown log type, module system should have caught this!";
        rfc5424 =
          optional (!time) "notime"
          ++ optional (!timeQuality) "notq"
          ++ optional (!host) "nohost";
      in
        ''
          export PATH=${utillinux}/bin:$PATH
          logger ${connection} \
            ${if rfc5424 == [] then "--rfc5424" else "--rfc5424=${concatStringsSep "," rfc5424}"} \
            ${optionalString (tag != null) "-t ${tag}"} \
            ${optionalString (priority != null) "-t ${priority}"} \
        ''
    else
      "cat"
   }
''
