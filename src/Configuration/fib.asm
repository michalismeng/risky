.text

.global _start
_start:
	addi x1, x0, 1
	add x2, x0, x0
	addi x3, x0, 1
	addi x5, x0, 10
	la x7, x
	lw x5, 0(x7)
	
loop:
	slt x6, x3, x5
	beq x6, x0, halt
	add x4, x2, x1
	add x1, x2, x0
	add x2, x4, x0
	addi x3, x3, 1
	beq x0, x0, loop
	
halt: 	beq x0, x0, halt

.data

y: .word 100, 150, 200
x: .word 12, 16, 20
