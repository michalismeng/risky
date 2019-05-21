.text

main:
	addi x1, x0, 1
	addi x2, x0, 2
	beq x1, x0, skip
	addi x3, x0, 3
	addi x4, x0, 4
	addi x5, x0, 5
	beq x0, x0, skip
	addi x6, x0, 6
	addi x7, x0, 7
	addi x8, x0, 8
skip:
	addi x9, x0, 9
halt: beq x0, x0, halt
