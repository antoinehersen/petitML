	.section __TEXT,__text,regular,pure_instructions
	.section __TEXT,__picsymbolstub1,symbol_stubs,pure_instructions,32
	.machine ppc
	.text
	.align 2
	.globl _petitml_entry
_petitml_entry:
	mflr r0
	stmw r30,-8(r1)
	stw r0,8(r1)
	stwu r1,-96(r1)
	mr r30,r1
	stw r3,120(r30)
	li r0,2
	stw r0,56(r30)
	lwz r2,120(r30)
	lwz r0,56(r30)
	add r0,r2,r0
	stw r0,120(r30)
	lwz r3,120(r30)
	bl L_toto$stub
	mr r0,r3
	stw r0,120(r30)
	lwz r0,120(r30)
	mr r3,r0
	lwz r1,0(r1)
	lwz r0,8(r1)
	mtlr r0
	lmw r30,-8(r1)
	blr
	.align 2
	.globl _toto
_toto:
	mflr r0
	stmw r30,-8(r1)
	stw r0,8(r1)
	stwu r1,-96(r1)
	mr r30,r1
	stw r3,120(r30)
	lwz r3,120(r30)
	bl _toto
	mr r0,r3
	stw r0,56(r30)
	lwz r2,120(r30)
	addi r0,r2,1
	mr r3,r0
	lwz r1,0(r1)
	lwz r0,8(r1)
	mtlr r0
	lmw r30,-8(r1)
	blr
	.section __TEXT,__picsymbolstub1,symbol_stubs,pure_instructions,32
	.align 5
L_toto$stub:
	.indirect_symbol _toto
	mflr r0
	bcl 20,31,"L00000000001$spb"
"L00000000001$spb":
	mflr r11
	addis r11,r11,ha16(L_toto$lazy_ptr-"L00000000001$spb")
	mtlr r0
	lwzu r12,lo16(L_toto$lazy_ptr-"L00000000001$spb")(r11)
	mtctr r12
	bctr
	.lazy_symbol_pointer
L_toto$lazy_ptr:
	.indirect_symbol _toto
	.long	dyld_stub_binding_helper
	.subsections_via_symbols
