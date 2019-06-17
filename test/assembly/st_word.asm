.data
	val: .word 0x12345678 0x456789ab
	
.text
main:
	la x1, val
    li x2, 0xDEADBABA
    li x3, 0x8BADF00D

    sw x2, 0(x1)
    sw x3, 4(x1)
    addi x1, x1, 8
    sw x3, 0(x1)

    addi x1, x1, -8
    lw x4, 0(x1)
    lw x5, 4(x1)
    lw x6, 8(x1)

halt:	beq x0, x0, halt
