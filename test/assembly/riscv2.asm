.data
	val: .word 0x12341234 0x45674567
	
.text
main:
	addi x1, x0, 0
	lw x2, 0(x1)
	lw x3, 4(x1)
	
	addi x1, x1, 4
	lw x4, -4(x1)
	lw x5, 0(x1)
	
halt:	beq x0, x0, halt
