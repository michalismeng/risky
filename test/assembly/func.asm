# Performs a function call. x3 and x4 should contain the sum of x1, x2. x3 is evaluated in the function.

.text
main:
	addi x1, x0, 10
	addi x2, x0, 25
	jal x5, func
	add x4, x1, x2

halt:	j halt

func:
	add x3, x1, x2
	jalr x0, x5, 0