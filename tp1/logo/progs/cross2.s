
_start:
	debug "main"
L0:
	reserve 2
	invoke 7
	push #0
	set_glob 0
	push #10
	set_glob 1
	get_glob 0
	invoke 3
	get_glob 1
	invoke 1
	push #30
	get_glob 0
	add
	set_glob 0
	push #10
	get_glob 0
	add
	set_glob 1
	get_glob 0
	invoke 3
	get_glob 1
	invoke 1
	push #30
	get_glob 0
	add
	set_glob 0
	push #10
	get_glob 0
	add
	set_glob 1
	get_glob 0
	invoke 3
	get_glob 1
	invoke 1
	push #30
	get_glob 0
	add
	set_glob 0
	push #10
	get_glob 0
	add
	set_glob 1
	get_glob 0
	invoke 3
	get_glob 1
	invoke 1
	stop
