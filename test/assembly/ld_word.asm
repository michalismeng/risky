# Load words from Data section
# Register x1 is used for indexing. Registers x2-x5 contain read values.

.data
	val: .word 0x12341234 0x45674567
	
.text
main:
	la x1, val
	lw x2, 0(x1)
	addi x4, x4, 1
	lw x3, 4(x1)
	
	addi x1, x1, 4
	addi x9, x0, 1
	lw x4, -4(x1)
	lw x5, 0(x1)
	
halt:	beq x0, x0, halt