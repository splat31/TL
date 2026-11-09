@
@ Exercice 4
@
@ Ecrivez un programme qui affiche une ligne brisée constituée de 20
@ modèles composés d’un segment montant (45°) de dimension 10 unité et
@ d’un segment descendant (45°) de dimension 10.
@

_start:
	reserve 1	@ 0 -> i
	push #0
	set_glob 0
L0:
	@verif i<10
	push #10
	get_glob 0 
	goto_ge L1

	push #45
	invoke 3 @rotation gauche
	push #10
	invoke 1 @avancer

	push #90
	invoke 3
	push #10
	invoke 1

	push #225
	invoke 3

	@ajoute 1 à i
	get_glob 0
	push #1
	add
	set_glob 0
	goto L0
L1:
	stop
