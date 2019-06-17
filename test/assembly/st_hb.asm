.data
	val: .word 0x12345678 0x456789ab
	
.text
main:
	la x1, val
    li x2, 0xDEADBABA
    li x3, 0x2BADB002

    sh x2, 0(x1)
    sh x3, 2(x1)
    sh x3, 5(x1)

    sb x2, 7(x1)

    lw x4, 0(x1)
    lw x5, 4(x1)

halt:	beq x0, x0, halt
