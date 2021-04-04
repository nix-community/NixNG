echo "<--- Stage 2.1 --->"

mkdir /tmp

# If running in a container create /etc/runit/stopit so that when a SIGCONT is received by runit, it shuts down
# And yes we're checking for one, because `(toString true)` for some bizarre reason evaluates to "1"
if [[ "@isContainer@" = "1" ]] ; then
    mkdir -p /etc/runit
    touch /etc/runit/stopit
    chmod 544 /etc/runit/stopit
fi

# Run activation script for this system
@activationScript@
