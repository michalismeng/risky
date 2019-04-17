# Caclulate the product of integers x1, x2. Store result at x3.
# This is done iteratively, since there is no MUL instruction in the Integer risc-v specification.
# In this program numbers are initialized to 5, 7.

.text
main:
	addi x1, x0, 5
	addi x2, x0, 7
	add  x3, x0, x0
	
	add x4, x0, x1
loop:
	beq x4, x0, halt
	add x3, x2, x3
	addi x4, x4, -1
	jal x0, loop

halt:	beq x0, x0, halt	