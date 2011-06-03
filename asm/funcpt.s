	.file	"funcpt.c"
	.section	".text"
	.align 2
	.globl do_addition
	.type	do_addition, @function
do_addition:
	stwu 1,-48(1)
	stw 31,44(1)
	mr 31,1
	stw 3,24(31)
	stw 4,28(31)
	lwz 9,24(31)
	lwz 0,28(31)
	add 0,9,0
	stw 0,8(31)
	lwz 0,8(31)
	mr 3,0
	lwz 11,0(1)
	lwz 31,-4(11)
	mr 1,11
	blr
	.size	do_addition,.-do_addition
	.align 2
	.globl do_substraction
	.type	do_substraction, @function
do_substraction:
	stwu 1,-48(1)
	stw 31,44(1)
	mr 31,1
	stw 3,24(31)
	stw 4,28(31)
	lwz 9,24(31)
	lwz 0,28(31)
	add 0,9,0
	stw 0,8(31)
	lwz 0,8(31)
	mr 3,0
	lwz 11,0(1)
	lwz 31,-4(11)
	mr 1,11
	blr
	.size	do_substraction,.-do_substraction
	.align 2
	.globl main
	.type	main, @function
main:
	stwu 1,-64(1)
	mflr 0
	stw 31,60(1)
	stw 0,68(1)
	mr 31,1
	stw 3,40(31)
	stw 4,44(31)
	li 0,100
	stw 0,24(31)
	li 0,200
	stw 0,20(31)
	lis 9,do_addition@ha
	la 0,do_addition@l(9)
	stw 0,12(31)
	lis 9,do_substraction@ha
	la 0,do_substraction@l(9)
	stw 0,8(31)
	lwz 0,12(31)
	mtctr 0
	lwz 3,24(31)
	lwz 4,20(31)
	bctrl
	mr 0,3
	stw 0,16(31)
	lwz 0,8(31)
	mtctr 0
	lwz 3,20(31)
	lwz 4,24(31)
	bctrl
	mr 0,3
	stw 0,16(31)
	li 0,0
	mr 3,0
	lwz 11,0(1)
	lwz 0,4(11)
	mtlr 0
	lwz 31,-4(11)
	mr 1,11
	blr
	.size	main,.-main
	.ident	"GCC: (GNU) 4.0.3 (Ubuntu 4.0.3-1ubuntu5)"
	.section	.note.GNU-stack,"",@progbits
