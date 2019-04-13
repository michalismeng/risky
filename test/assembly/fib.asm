.text

main:
	addi x1, x0, 1
	add x2, x0, x0
	addi x3, x0, 1
	addi x5, x0, 6
	
loop:
	slt x6, x3, x5
	beq x6, x0, halt
	add x4, x2, x1
	add x1, x2, x0
	add x2, x4, x0
	addi x3, x3, 1
	j loop
	
halt: 	j halt