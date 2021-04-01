export PATH=@runit@/bin:@findutils@/bin:@busybox@/bin

mkdir -p @runtimeServiceDirectory@

function linkFarm() {
    src="$1"
    dst="$2"

    find "$src" -mindepth 1 -type d -printf "%P\n" | xargs -I {} mkdir "$dst/{}"
    find "$src" -mindepth 1 -type f -printf "%P\n" | xargs -I {} ln -s "$src/{}" "$dst/{}"
    find "$src" -mindepth 1 -type l -printf "%P\n" | xargs -I {} cp "$src/{}" "$dst/{}"
}

linkFarm @serviceDir@ @runtimeServiceDirectory@

runsvdir @runtimeServiceDirectory@
