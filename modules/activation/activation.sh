# Check whether NixOS or another distro is installed
ANOTHER_DISTRO=$(
    if [[ -f /etc/os-release ]] ; then
        source /etc/os-release
        
        if [[ -e "$NAME" "NixSmth" ]] && [[ -e "$ID" "nixsmth" ]] ; then
            false
            echo $?
        else
            true
            echo $?
        fi
    else
        false
        echo $?
    fi
	      )

if [[ "$ANOTHER_DISTRO" == "0" ]] ; then
    echo "Refusing to activate when another distro is installed!"
    exit 1     
fi

# ${concatMapStringsSep "\n"
#   (file:
#    let
#    cmd = if file.copy then "cp" else "ln -s";
#    in
#        ''
#        mkdir -p $(dirname ${file.destination})
#        ${cmd} ${file.source} ${file.destination}
#        ''
#   )
#   config.system.environment.files
# }
