# Load half-words and bytes from Data section
# Register x1 is used for indexing (always 4-byte aligned). Registers x2-x9 contain read values.

.data
	val: .word 0x12345678 0x456789ab
	
.text
main:
	la x1, val
	lh x2, 0(x1)
	lh x3, 2(x1)
	lh x8, 4(x1)
	lh x9, 6(x1)
	
	addi x1, x1, 4
	lb x4, 0(x1)
	lb x5, 1(x1)
	lb x6, 2(x1)
	lb x7, 3(x1)
	
halt:	beq x0, x0, halt
