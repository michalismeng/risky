.text
main:
	addi x1, x0, 5
	addi x2, x0, 1
	add  x3, x0, x0
loop:
	add x3, x2, x3
	addi x2, x2, 1
	addi x1, x1, -1
	bne x1, x0, loop
	
halt:	beq x0, x0, halt
