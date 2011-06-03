	.text
.globl _petitML_entry
_petitML_entry:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	$10, %eax
	leave
	ret
	.subsections_via_symbols
