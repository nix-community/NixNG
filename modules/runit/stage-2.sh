export PATH=@runit@/bin:@busybox@/bin

echo "<--- Stage 2.2 --->"

unset _system_config
runsvdir @runtimeServiceDirectory@
