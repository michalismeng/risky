.text

main:
	lui x1, 264209
	auipc x2, 15
	addi x3, x2, 1
	
	add x0, x0, x0
	add x0, x0, x0
	add x0, x0, x0
	
halt: 	beq x0, x0, halt
