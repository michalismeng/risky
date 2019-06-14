# Load half-word and bytes that should be sign extended or zero extended

.data
	val: .word 0x12345688 0xFFFFFFFF
	
.text
main:
	la x1, val

	lh x2, 4(x1)        # sign-extend with 1
    lhu x3, 4(x1)       # zero-extend
    lb x4, 4(x1)        # sign-extend with 1
    lbu x5, 4(x1)       # zero-extend

    lh x6, 0(x1)        # sign-extend with 0
    lhu x7, 0(x1)       # zero-extend
    lb x8, 0(x1)        # sign-extend with 1
    lbu x9, 0(x1)       # zero-extend
	
	
halt:	j halt
