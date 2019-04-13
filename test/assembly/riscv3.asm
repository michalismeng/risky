.data
	val: .word 0x12341234 0x45674567
	
.text
main:
	addi x1, x0, 0
	lh x2, 0(x1)
	lh x3, 4(x1)
	
	addi x1, x1, 4
	lb x4, 0(x1)
	lb x5, 1(x1)
	
halt:	beq x0, x0, halt