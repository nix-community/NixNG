export PATH=@runit@/bin:@busybox@/bin

echo "<--- Stage 2.2 --->"

(
@readOnlyStore@
)

# Below this line GPL continues

unset _system_config
runsvdir @runtimeServiceDirectory@
