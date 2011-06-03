# PetitML

## Introduction
We have implemented a small functional language. The compiler emit PPC assembly code that is linked with a small runtime library in C.

The most difficult part was to conform to the System V PPC application binary interface in order to be able to take advantage of C functions.

The programme make use of monad in several occasion. The obvious case as alternate mean of computation in the intermediate interpreter present for testing, but also for AST traversal when carrying states.

## Programme
The programme is divided in different modules.

### Syntax
The syntax is described in this module.

### K-normal
Apply the first normalisation. Contrary to the common choice of the CPS transformation K-normal does not produce a large number of administrative lambdas.
There is an interpreter for K-normalised programme for testing purpose.

### Eta
The eta transformation facilitate the programme manipulation.

### Closure
The closure conversion is necessary as function are first order.

### PPC asm
Generation of PPC assembly code.

## Example

Output example for the programme 3 described in the Testing module.

<code>
  .file   "petitML.pml"
   .section        ".text"
   .align 2
   .global f1
   .type  , @function
f1:
   stwu 1,-120(1)
   mflr 0
   stw 0,124(1)
   mr 6,3
   mr 7,4
   lwz 8,4(3)
   mr 5,8
   mr 3,5
   lwz 2,0(1)
   lwz 0,4(2)
   mtlr 0
   mr 1,2
   blr
   .size  f1,.-f1
   .align 2
   .global petitML_entry
   .type  , @function
petitML_entry:
   stwu 1,-120(1)
   mflr 0
   stw 0,124(1)
   mr 31,3
   li  9,0
   mr 10,9
   cmpwi 3,10,0
   bne 3,.L1
   li  11,12
   li  12,1
   mr 15,31
   lis  2,f1@ha
   la 0,f1@l(2)
   stw 0,0(31)
   stw 11,4(31)
   addi 0,31,8
   mr 14,15
   li  16,2
   mr 15,16
   stmw 5,8(1)
   lwz 2,0(14)
   mtctr 2
   mr 4,15
   mr 3,14
   bctrl
   mr 0,31
   lmw 5,8(1)
   mr 31,0
   mr 13,3
   add 8,12,13
   b .L2
.L1:
   li  8,200
.L2:
   li  9,3
   mr 10,8
   add 5,9,10
   mr 3,5
   lwz 2,0(1)
   lwz 0,4(2)
   mtlr 0
   mr 1,2
   blr
   .size  petitML_entry,.-petitML_entry
   .ident  "Super petit Ml compiler"
   .section        .note.GNU-stack,"",@progbits

</code>
