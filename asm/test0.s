	.file	"test0.c"
	.section	".text"
	.align 2
	.globl main
	.type	main, @function
main:
	stwu 1,-48(1)
	stw 31,44(1)
	mr 31,1
	stw 3,12(31)
	stw 4,8(31)
	lis 0,0x1ed
	ori 0,0,12995
	stw 0,24(31)
	lwz 0,24(31)
	cmpwi 7,0,0
	bne 7,.L2
	lwz 9,24(31)
	lwz 0,24(31)
	add 0,9,0
	stw 0,24(31)
	li 0,0
	stw 0,24(31)
	lwz 9,24(31)
	addi 0,9,-3
	stw 0,24(31)
	b .L4
.L2:
	lwz 9,28(31)
	lwz 0,24(31)
	add 0,9,0
	stw 0,28(31)
	lwz 0,28(31)
	slwi 0,0,1
	stw 0,28(31)
.L4:
	lwz 9,24(31)
	lwz 0,28(31)
	add 0,9,0
	stw 0,24(31)
	li 0,0
	mr 3,0
	lwz 11,0(1)
	lwz 31,-4(11)
	mr 1,11
	blr
	.size	main,.-main
	.ident	"GCC: (GNU) 4.1.2 20060928 (prerelease) (Ubuntu 4.1.1-13ubuntu5)"
	.section	.note.GNU-stack,"",@progbits
