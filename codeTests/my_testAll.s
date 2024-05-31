			# This code was produced by the CERI Compiler enhanced by colin.palazzetti-rubera@alumni.univ-avignon.fr
	.data
	.align 8
_FormatStringInt:	 .string "%d "
_FormatStringUInt:	 .string "%llu "
_FormatStringDouble:	 .string "%lf "
_FormatStringChar:	 .string "%c "
_StringTrue:	 .string "TRUE "
_StringFalse:	 .string "FALSE "
_EmptyString:	 .string ""
_stackTop:	 .quad 0
a:	.quad 0 # INTEGER
b:	.quad 0 # INTEGER
c:	.quad 0 # INTEGER
d:	.quad 0 # INTEGER
i:	.quad 0 # INTEGER
j:	.quad 0 # INTEGER
k:	.quad 0 # INTEGER
x:	.double 0.0 # DOUBLE
y:	.double 0.0 # DOUBLE
flag:	.quad 0 # BOOLEAN
ch:	.byte 0 # CHAR
	.text		# The following lines contain the program
	.globl main	# The main function must be visible from outside
	.cfi_startproc	# The main function must be visible from outside
main:			# The main function body :
	movq %rsp, _stackTop	# Save the position of the stack's top
Display1:
	push ch
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $_FormatStringChar, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
Display1End:	movq $_EmptyString, %rdi
	call puts@PLT
	push $3
	pop a
	push $0
	pop b
	push $0
	pop c
	push $0
	pop d
	subq	$8, %rsp			# Going to add on stack top
	movl	$0, (%rsp)	# 32-bit high part of 0
	movl	$0, 4(%rsp)	# 32-bit low part of 0
	pop x
	subq	$8, %rsp			# Going to add on stack top
	movl	$0, (%rsp)	# 32-bit high part of 1.5
	movl	$1073217536, 4(%rsp)	# 32-bit low part of 1.5
	pop y
	push $-1
	pop flag
	movq	$0, %rax
	movb	$65, %al
	push	%rax	# push a 8-byte version of 'A'
	pop %rax
	movb %al, ch
ForAssign2:
	push $1
	pop j
ForTo2:
	push $10
ForTest2:	 movq (%rsp), %rax
	cmpq %rax, j
	ja ForEnd2	# si varBoucle > i
IF3:
	push j
	push $2
	pop %rbx
	pop %rax
	movq $0, %rdx
	div %rbx
	push %rdx	# MOD
	push $0
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	je _Vrai4	# If equal
	push $0		# False
	jmp _Suite4
_Vrai4:	push $0xFFFFFFFFFFFFFFFF		# True
_Suite4:
IFCHECK3:	 popq %rax
	cmpq $0, %rax
	je IFELSE3
IFTHEN3:
ForAssign5:
	push $1
	pop k
ForTo5:
	push j
ForTest5:	 movq (%rsp), %rax
	cmpq %rax, k
	ja ForEnd5	# si varBoucle > i
IF6:
	push a
	push $1
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	je _Vrai7	# If equal
	push $0		# False
	jmp _Suite7
_Vrai7:	push $0xFFFFFFFFFFFFFFFF		# True
_Suite7:
IFCHECK6:	 popq %rax
	cmpq $0, %rax
	je IFELSE6
IFTHEN6:
	push b
	push $1
	pop %rbx
	pop %rax
	addq  %rbx, %rax	# ADD
	push %rax
	pop b
	jmp IFEND6
IFELSE6:
	push b
	push $2
	pop %rbx
	pop %rax
	addq  %rbx, %rax	# ADD
	push %rax
	pop b
	push c
	push $1
	pop %rbx
	pop %rax
	addq  %rbx, %rax	# ADD
	push %rax
	pop c
	push x
	push y
	pop %rbx
	pop %rax
	addq  %rbx, %rax	# ADD
	push %rax
	pop x
IF8:
	push ch
	movq	$0, %rax
	movb	$65, %al
	push	%rax	# push a 8-byte version of 'A'
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	je _Vrai9	# If equal
	push $0		# False
	jmp _Suite9
_Vrai9:	push $0xFFFFFFFFFFFFFFFF		# True
_Suite9:
IFCHECK8:	 popq %rax
	cmpq $0, %rax
	je IFELSE8
IFTHEN8:
	movq	$0, %rax
	movb	$66, %al
	push	%rax	# push a 8-byte version of 'B'
	pop %rax
	movb %al, ch
	jmp IFEND8
IFELSE8:
	movq	$0, %rax
	movb	$65, %al
	push	%rax	# push a 8-byte version of 'A'
	pop %rax
	movb %al, ch
IFEND8:
IFEND6:
ForInc5:
	incq k
	jmp ForTest5
ForEnd5:
	jmp IFEND3
IFELSE3:
	push j
	pop i
While10:
	push i
	push $0
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	ja _Vrai11	# If above
	push $0		# False
	jmp _Suite11
_Vrai11:	push $0xFFFFFFFFFFFFFFFF		# True
_Suite11:
WhileCheck10:	 popq %rax
	cmpq $0, %rax
	je WhileEnd10
IF12:
	push flag
IFCHECK12:	 popq %rax
	cmpq $0, %rax
	je IFELSE12
IFTHEN12:
	push b
	push $1
	pop %rbx
	pop %rax
	addq  %rbx, %rax	# ADD
	push %rax
	pop b
	push $0
	pop flag
	jmp IFEND12
IFELSE12:
	push b
	push $2
	pop %rbx
	pop %rax
	addq  %rbx, %rax	# ADD
	push %rax
	pop b
	push c
	push $1
	pop %rbx
	pop %rax
	addq  %rbx, %rax	# ADD
	push %rax
	pop c
	push y
	subq	$8, %rsp			# Going to add on stack top
	movl	$2576980378, (%rsp)	# 32-bit high part of 1.1
	movl	$1072798105, 4(%rsp)	# 32-bit low part of 1.1
	fldl (%rsp)
	addq $8, %rsp	# popq
	fldl (%rsp)
	# addq $8, %rsp	# not popq : result will be stored
	fmul %st(1)
	fstpl (%rsp)	# retrieve st0
	fmulp %st(0)	# Depop
	pop y
	push $-1
	pop flag
IFEND12:
	push i
	push $1
	pop %rbx
	pop %rax
	subq  %rbx, %rax	# SUB
	push %rax
	pop i
	jmp While10
WhileEnd10:
IFEND3:
ForAssign13:
	push $1
	pop i
ForTo13:
	push $5
ForTest13:	 movq (%rsp), %rax
	cmpq %rax, i
	ja ForEnd13	# si varBoucle > i
IF14:
	push i
	push $2
	pop %rbx
	pop %rax
	movq $0, %rdx
	div %rbx
	push %rdx	# MOD
	push $1
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	je _Vrai15	# If equal
	push $0		# False
	jmp _Suite15
_Vrai15:	push $0xFFFFFFFFFFFFFFFF		# True
_Suite15:
IFCHECK14:	 popq %rax
	cmpq $0, %rax
	je IFELSE14
IFTHEN14:
	push $-1
	pop flag
IF16:
	push flag
IFCHECK16:	 popq %rax
	cmpq $0, %rax
	je IFELSE16
IFTHEN16:
	push d
	push i
	pop %rbx
	pop %rax
	addq  %rbx, %rax	# ADD
	push %rax
	pop d
	jmp IFEND16
IFELSE16:
	push d
	push i
	pop %rbx
	pop %rax
	subq  %rbx, %rax	# SUB
	push %rax
	pop d
IFEND16:
	jmp IFEND14
IFELSE14:
IFEND14:
ForAssign17:
	push $1
	pop a
ForTo17:
	push $3
ForTest17:	 movq (%rsp), %rax
	cmpq %rax, a
	ja ForEnd17	# si varBoucle > i
	movq	$0, %rax
	movb	$65, %al
	push	%rax	# push a 8-byte version of 'A'
	pop %rax
	movb %al, ch
IF18:
	push ch
	movq	$0, %rax
	movb	$65, %al
	push	%rax	# push a 8-byte version of 'A'
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	je _Vrai19	# If equal
	push $0		# False
	jmp _Suite19
_Vrai19:	push $0xFFFFFFFFFFFFFFFF		# True
_Suite19:
IFCHECK18:	 popq %rax
	cmpq $0, %rax
	je IFELSE18
IFTHEN18:
	push b
	push $1
	pop %rbx
	pop %rax
	addq  %rbx, %rax	# ADD
	push %rax
	pop b
	jmp IFEND18
IFELSE18:
IF20:
	push ch
	movq	$0, %rax
	movb	$66, %al
	push	%rax	# push a 8-byte version of 'B'
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	je _Vrai21	# If equal
	push $0		# False
	jmp _Suite21
_Vrai21:	push $0xFFFFFFFFFFFFFFFF		# True
_Suite21:
IFCHECK20:	 popq %rax
	cmpq $0, %rax
	je IFELSE20
IFTHEN20:
	push c
	push $1
	pop %rbx
	pop %rax
	addq  %rbx, %rax	# ADD
	push %rax
	pop c
	jmp IFEND20
IFELSE20:
	push d
	push $1
	pop %rbx
	pop %rax
	addq  %rbx, %rax	# ADD
	push %rax
	pop d
IFEND20:
IFEND18:
ForInc17:
	incq a
	jmp ForTest17
ForEnd17:
ForInc13:
	incq i
	jmp ForTest13
ForEnd13:
ForInc2:
	incq j
	jmp ForTest2
ForEnd2:
	push $0
	pop i
_22_Repeat:
Display23:
	push i
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
Display23End:	movq $_EmptyString, %rdi
	call puts@PLT
	push i
	push $1
	pop %rbx
	pop %rax
	addq  %rbx, %rax	# ADD
	push %rax
	pop i
	push i
	push $4
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	je _Vrai24	# If equal
	push $0		# False
	jmp _Suite24
_Vrai24:	push $0xFFFFFFFFFFFFFFFF		# True
_Suite24:
_22_RepeatTest:	 popq %rax
	cmpq $0, %rax
	je _22_Repeat
_22_RepeatEnd:
Display25:
	push ch
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $_FormatStringChar, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
Display25End:	movq $_EmptyString, %rdi
	call puts@PLT
Display26:
	push $0
	movq $0, %rax
	pop %rax 	# The value to be displayed
	cmpq $0, %rax
	je DisplayFalse26_1
	movq $_StringTrue, %rdi
	call	printf@PLT
	nop	# Prevent problems with printf
	jmp Display26_1End
DisplayFalse26_1:	 movq $_StringFalse, %rdi
	call	printf@PLT
	nop	# Prevent problems with printf
Display26_1End:
Display26End:	movq $_EmptyString, %rdi
	call puts@PLT
Display27:
	movq $_EmptyString, %rdi
	call puts@PLT
Display27End:	movq $_EmptyString, %rdi
	call puts@PLT
	movq _stackTop, %rsp		# Restore the position of the stack's top
	nop
	ret			# Return from main function
	.cfi_endproc
