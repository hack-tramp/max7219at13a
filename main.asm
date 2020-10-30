.include "tn13adef.inc"
.dseg
.org SRAM_START
matrix:	.byte	8	;LED matrix in ram
.cseg
.org 0x00
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
#define waitfor 50
;note: r17 is used for looping

rcall init



mainlp:

	;select char a
	ldi	ZL,LOW(2*char_a)		; initialize Z pointer
	ldi	ZH,HIGH(2*char_a)		; to pmem array address
	rcall load_char ;load it to ram
	rcall draw_ram

	ldi r18,waitfor
	rcall wait_time

	;select char c
	ldi	ZL,LOW(2*char_c)		; initialize Z pointer
	ldi	ZH,HIGH(2*char_c)		; to pmem array address
	rcall load_char ;load it to ram
	rcall draw_ram

	ldi r18,waitfor
	rcall wait_time

	;select char a
	ldi	ZL,LOW(2*char_a)		; initialize Z pointer
	ldi	ZH,HIGH(2*char_a)		; to pmem array address
	rcall load_char ;load it to ram
	rcall draw_ram

	ldi r18,waitfor
	rcall wait_time

	;select char b
	ldi	ZL,LOW(2*char_b)		; initialize Z pointer
	ldi	ZH,HIGH(2*char_b)		; to pmem array address
	rcall load_char ;load it to ram
	rcall draw_ram

	ldi r18,waitfor
	rcall wait_time

	;or just clear_max
	rcall clear_ram
	rcall draw_ram

	ldi r18,waitfor
	rcall wait_time

	rjmp mainlp


init:
	ldi	XL,LOW(matrix)		; initialize pointer
	ldi	XH,HIGH(matrix)		; to matrix address in ram
	;default char
	ldi	ZL,LOW(2*char_a)		; initialize Z pointer
	ldi	ZH,HIGH(2*char_a)		; to pmem array address

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

	ret

load_char:
	ldi	XL,LOW(matrix)		; initialize pointer
	ldi	XH,HIGH(matrix)		; to matrix address in ram
	ldi r17,8
	arrLp:	
		lpm	r16,Z+			; load value from pmem array
		st X+,r16			; store value to SRAM array
		dec	r17			; decrement loop count
		brne arrLp			; repeat loop for all bytes in array
	ret

draw_ram:
	ldi	XL,LOW(matrix)		; initialize pointer
	ldi	XH,HIGH(matrix)		; to matrix address in ram
	ldi r22,0 ; second counter reg as we use r17 for sending
	draw_loop:
		inc r22
		mov r18,r22
		ld r19,X+
		rcall max_send
		cpi r22,8
		brne draw_loop
	ret

clear_ram:
	ldi XL,LOW(matrix); reset pointer to first matrix byte
	ldi XH,HIGH(matrix)
	ldi r17,8
	ldi r16,0
	clear_loop:
		st X+, r16
        dec r17
		brne clear_loop        ;    do it 8 times
	ret

clear_max:
	ldi r22,0 ; second counter reg as we use r17 for sending
	drw_loop:
		inc r22
		mov r18,r22
		ldi r19,0
		rcall max_send
		cpi r22,8
		brne drw_loop
	ret

wait_time:
    ldi r16,0                   ; these are timer counters
    ldi r17,0
	timer2:
		inc r16                     ; do 256 iterations - 1 clock
		brne timer2					; branch if not equal to beginning of timer2 - 1 clock * 256, then 1
		inc r17                     ; do 256 times - 1 clock
		brne timer2					; branch if not equal to beginning of timer2 - 1 clock * 256, then 1
		dec r18						; do 5 times - 1 clock
		brne timer2                 ; branch if not equal to beginning of timer2 - 1 clock * 5, then 1
    ret                         ; once there have been 256 * 256 * 5 loops, return                      ; once there have been 256 * 256 * 5 loops, return


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


max_send: ; params r18 reg/row, r19 data
	rcall cs_high
	mov r16,r18
	rcall max_write
	mov r16,r19
	rcall max_write
	rcall cs_low
	nop
	rcall cs_high
	ret


char_a: .db 0b11111111,0b10000001,0b10000001,0b11111111,0b10000001,0b10000001,0b10000001,0b10000001 
char_b: .db 0b11111110,0b10000001,0b10000001,0b11111110,0b10000001,0b10000001,0b10000001,0b11111110 
char_c: .db 0b11111111,0b10000000,0b10000000,0b10000000,0b10000000,0b10000000,0b10000000,0b11111111 
