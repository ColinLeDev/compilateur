			# This code was produced by the CERI Compiler enhanced by colin.palazzetti-rubera@alumni.univ-avignon.fr
	.data
	.align 8
_FormatStringInt:	 .string "%d "
_FormatStringUInt:	 .string "%llu "
_FormatStringDouble:	 .string "%lf "
_FormatStringChar:	 .string "%c"
_StringTrue:	 .string "TRUE "
_StringFalse:	 .string "FALSE "
_EmptyString:	 .string ""
_stackTop:	 .quad 0
	.text		# The following lines contain the program
	.globl main	# The main function must be visible from outside
	.cfi_startproc	# The main function must be visible from outside
main:			# The main function body :
	movq %rsp, _stackTop	# Save the position of the stack's top
Display1:
	movq $_EmptyString, %rdi
	call puts@PLT
	movq	$0, %rax
	movb	$72, %al
	push	%rax	# push a 8-byte version of 'H'
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $_FormatStringChar, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
	movq	$0, %rax
	movb	$101, %al
	push	%rax	# push a 8-byte version of 'e'
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $_FormatStringChar, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
	movq	$0, %rax
	movb	$108, %al
	push	%rax	# push a 8-byte version of 'l'
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $_FormatStringChar, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
	movq	$0, %rax
	movb	$108, %al
	push	%rax	# push a 8-byte version of 'l'
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $_FormatStringChar, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
	movq	$0, %rax
	movb	$111, %al
	push	%rax	# push a 8-byte version of 'o'
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $_FormatStringChar, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
	movq	$0, %rax
	movb	$32, %al
	push	%rax	# push a 8-byte version of ' '
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $_FormatStringChar, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
	movq	$0, %rax
	movb	$87, %al
	push	%rax	# push a 8-byte version of 'W'
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $_FormatStringChar, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
	movq	$0, %rax
	movb	$111, %al
	push	%rax	# push a 8-byte version of 'o'
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $_FormatStringChar, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
	movq	$0, %rax
	movb	$114, %al
	push	%rax	# push a 8-byte version of 'r'
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $_FormatStringChar, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
	movq	$0, %rax
	movb	$108, %al
	push	%rax	# push a 8-byte version of 'l'
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $_FormatStringChar, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
	movq	$0, %rax
	movb	$100, %al
	push	%rax	# push a 8-byte version of 'd'
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $_FormatStringChar, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
	movq	$0, %rax
	movb	$33, %al
	push	%rax	# push a 8-byte version of '!'
	movq $0, %rax
	pop %rsi 	# The value to be displayed
	movq $_FormatStringInt, %rdi
	movq $_FormatStringChar, %rdi
	movq $0, %rax
	call printf@PLT
	nop	# Prevent problems with printf
Display1End:	movq $_EmptyString, %rdi
	call puts@PLT
Display2:
	movq $_EmptyString, %rdi
	call puts@PLT
Display2End:	movq $_EmptyString, %rdi
	call puts@PLT
	movq _stackTop, %rsp		# Restore the position of the stack's top
	nop
	ret			# Return from main function
	.cfi_endproc
