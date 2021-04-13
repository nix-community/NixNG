export PATH=@runit@/bin:@findutils@/bin:@busybox@/bin

echo "<--- Stage 2.2 --->"

runsvdir @runtimeServiceDirectory@
