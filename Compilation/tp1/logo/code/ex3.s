@
@ Exercice 3
@
@ écrivez un programme qui affiche les 10 premiers entiers, entre 0 et 9,
@ ainsi que leur somme.
@

_start:
	reserve 2	@ 0 -> i, 1 -> s
L0:
	push #0
	set_glob 0
	push #0
	set_glob 1
L1:
	@verif i<10
	push #10
	get_glob 0 
	goto_ge L2

	@affiche i
	get_glob 0
	invoke 5

	@calcul la somme et l'affiche
	get_glob 1
	get_glob 0
	add
	set_glob 1
	get_glob 1
	invoke 5


	@ajoute 1 à i
	get_glob 0
	push #1
	add
	set_glob 0


	goto L1
L2:
	stop
