# Load half-words that are unaligned

.data
	val: .word 0x12345678 0x456789ab
	
.text
main:
	la x1, val
	lh x2, 1(x1)
	
	
halt:	beq x0, x0, halt
