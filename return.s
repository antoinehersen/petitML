	.section __TEXT,__text,regular,pure_instructions
	.section __TEXT,__picsymbolstub1,symbol_stubs,pure_instructions,32
	.machine ppc
	.text
	.align 2
	.globl _main
_main:
	stw r3,24(r1)
	stw r4,28(r1)
	li r0,0
	mr r3,r0
	blr
	.subsections_via_symbols
