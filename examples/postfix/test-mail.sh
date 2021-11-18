# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{
    cat <<EOF
EHLO redalder.org
MAIL FROM:<root@redalder.org>
RCPT TO:<webmaster@example.org>
DATA
Subject: Hello

hello world
.

EOF

sleep 1
} | tee /dev/tty | telnet localhost 1025 
