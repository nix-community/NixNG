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
