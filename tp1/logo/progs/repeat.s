
_start:
	debug "main"
L0:
	reserve 1
	push #4
	set_glob 0
L1:
	get_glob 0
	push #0
	goto_le L2
	push #50
	invoke 1
	push #90
	invoke 3
	get_glob 0
	push #1
	sub
	set_glob 0
	goto L1
L2:
	stop
