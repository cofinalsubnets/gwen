n=mitty
x=mi
test: bin/host/$n
	$< test/*.$x
.PHONY: test
include build.mk
include install.mk
include misc.mk
