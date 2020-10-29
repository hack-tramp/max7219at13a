.include "tn13adef.inc"
.dseg
.org SRAM_START
matrix:	.byte	8	
.cseg

#define MAX7219_REG_NOOP 0x00
#define MAX7219_REG_DIGIT0 0x01
#define MAX7219_REG_DIGIT1 0x02
#define MAX7219_REG_DIGIT2 0x03
#define MAX7219_REG_DIGIT3 0x04
#define MAX7219_REG_DIGIT4 0x05
#define MAX7219_REG_DIGIT5 0x06
#define MAX7219_REG_DIGIT6 0x07
#define MAX7219_REG_DIGIT7 0x08
#define MAX7219_REG_DECODEMODE 0x09
#define MAX7219_REG_INTENSITY 0x0A
#define MAX7219_REG_SCANLIMIT 0x0B
#define MAX7219_REG_SHUTDOWN 0x0C
#define MAX7219_REG_DISPLAYTEST 0x0F

#define DIO_PIN PB0 ; PB0 
#define CLK_PIN PB1 ; PB1 
#define CS_PIN PB2 ; PB2

;note: r17 is used for looping
rcall init

mainlp:
	rjmp mainlp


init:
	ldi	ZL,LOW(matrix)		; initialize Z pointer
	ldi	ZH,HIGH(matrix)		; to matrix address

	ldi r16,0b00000111
	out DDRB,r16

	ldi r18,MAX7219_REG_DECODEMODE
	ldi r19,0x00
    rcall max_send
	ldi r18,MAX7219_REG_SCANLIMIT
	ldi r19,0x07
    rcall max_send
	ldi r18,MAX7219_REG_INTENSITY
	ldi r19,0x0f
    rcall max_send
	ldi r18,MAX7219_REG_DISPLAYTEST
	ldi r19,0x00
    rcall max_send
	ldi r18,MAX7219_REG_SHUTDOWN
	ldi r19,0x01
    rcall max_send

	ldi r18,0x04
	ldi r19,0x00
	rcall max_send


cs_high: 
	sbi PORTB, CS_PIN
	ret
cs_low:
	cbi PORTB, CS_PIN
	ret
clk_high:
	sbi PORTB, CLK_PIN
	ret
clk_low:
	cbi PORTB, CLK_PIN
	ret
data_high:
	sbi PORTB, DIO_PIN
	ret
data_low:
	cbi PORTB, DIO_PIN
	ret

max_write: ; param r16 input byte
	nop
	ldi r17,8 ;do this 8 times
	cycle:
		rcall clk_low
		nop
		sbrs r16,7 ;skip if bit 7 in register is set
		rcall data_low
		sbrc r16,7
		rcall data_high
		rcall clk_high 
		add r16,r16
		dec r17
		brne cycle
	ret


max_send: ; params r18 reg, r19 data
	rcall cs_high
	mov r16,r18
	rcall max_write
	mov r16,r19
	rcall max_write
	rcall cs_low
	nop
	rcall cs_high
	ret


max_set_pixel: ;params r18 row, r19 col, r20 bool value
	cpi r18,8 ; (signed) if bigger than 7
	brsh ms_end
	cpi r19,8
	brge ms_end

	mov r17,r19 ; 1 << col (r19)
	bitwise_loop:	
		lsl r19
		dec r17
		brne bitwise_loop
	mov r17,r18 ; loop amount of rows (1st param)
	ptr_loop:
		inc r30 ; manually increase high and low Z regs so we dont need to st/ld every time
		inc r31 ;move Z pointers to desired matrix element/byte/col
		dec r17
		brne ptr_loop
	clr r21
	ld r21,Z
	cpi r20,1
	brne bit_off
	or r21,r19
	bit_off:
		com r19 ;one's compliment
		and r21,r19
	st Z,r21

	inc r18
	clr r19
	mov r19,r21
	rcall max_send

	ldi ZL,LOW(matrix); reset pointer to first matrix byte
	ldi ZH,HIGH(matrix)
	ms_end:
	ret

max_clear:
	ldi r17,8
	clear_loop:
		st Z+, r0 
        dec r17
		brne clear_loop        ;    do it 8 times
	ldi ZL,LOW(matrix); reset pointer to first matrix byte
	ldi ZH,HIGH(matrix)
	ret