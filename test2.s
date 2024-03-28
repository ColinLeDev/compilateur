			# This code was produced by the CERI Compiler
	.text		# The following lines contain the program
	.globl main	# The main function must be visible from outside
main:			# The main function body :
	movq %rsp, %rbp	# Save the position of the stack's top
	push $3
	push $9
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	jb Vrai	# Aller sur code de vrai
Faux:	push $0	# Faux sur la pile
	jmp FinExp	# Aller à Fin, évider code faux
Vrai:	push $-1	# Code de vrai
FinExp:	movq %rbp, %rsp		# Restore the position of the stack's top
	ret			# Return from main function
