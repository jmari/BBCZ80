;	TITLE	BBCDIST.Z80 (C) R.T.RUSSELL 1982-2024
;
;BBC BASIC (Z80) - CP/M VERSION 2.20 & 3.00
;(C) COPYRIGHT R.T.RUSSELL, 1982-2024.
;ALL RIGHTS RESERVED.
;
;THIS PROGRAM ALLOWS THE USER TO ADAPT BBC BASIC TO THE
;PARTICULAR CHARACTERISTICS OF HIS SYSTEM HARDWARE ETC.
;
;THE PROGRAM RESIDES AT 100H FOR EASE OF LOADING.
;*** IT MUST NOT EXCEED 256 BYTES IN TOTAL LENGTH ***
;
;PLEASE NOTE THAT A Z80 PROCESSOR & CP/M VERSION 2.2
;OR LATER ARE REQUIRED.
;
;R.T.RUSSELL, 11-03-1984, 03-05-1989, 12-05-2024
;
CPM	EQU	5
COLD	EQU	200H
;
	PUBLIC	CLRSCN
	PUBLIC	PUTCSR
	PUBLIC	GETCSR
	PUBLIC	PUTIME
	PUBLIC	GETIME
	PUBLIC	GETKEY
	PUBLIC	BYE
;
;	ASEG
	ORG	100H
;
;JUMP TABLE - BASIC makes calls to hardware-dependent
;features via this table:
;
	JP	INIT
CLRSCN:	JP	CLS		;CLEAR SCREEN
PUTCSR:	JP	PCSR		;SET CURSOR POSN.
GETCSR:	JP	GCSR		;READ CURSOR POSN.
PUTIME:	JP	PTIME		;SET ELAPSED TIME
GETIME:	JP	GTIME		;READ ELAPSED TIME
GETKEY:	JP	INKEY		;READ KEY (TIME LIMIT)
BYE:	JP	REBOOT		;RETURN TO CP/M
;
;BDOS	- Save the IX & IY registers & before performing a
;	  CP/M function call.
;
BDOS:	PUSH	IX
	PUSH	IY
	CALL	CPM
	POP	IY
	POP	IX
	RET
;
;INIT	- Perform hardware initialisation (if any).
;
INIT:	LD	A,2
	INC	A
	LD	DE,NOTZ80
	JP	PE,FAIL
	LD	C,12
	CALL	BDOS
	OR	A
	LD	DE,NOTV2
	JP	NZ,COLD
FAIL:	LD	C,9
	CALL	BDOS
	RST	0
;
NOTZ80:	DEFB	"Not a Z80$"
NOTV2:	DEFB	"Not CP/M 2$"
;
;REBOOT	- Switch off interrupts & return to CP/M
;
REBOOT:	RST	0
;
;GTIME	- Read elapsed-time clock.
;  	  Outputs: DEHL = elapsed time (centiseconds)
; 	  Destroys: A,D,E,H,L,F
;
GTIME:	PUSH	BC
	CALL	TICKS
	LD	BC,(OFFLO)
	ADD	HL,BC
	EX	DE,HL
	LD	BC,(OFFHI)
	ADC	HL,BC
	EX	DE,HL
	POP	BC
	RET
;
;PTIME	- Load elapsed-time clock.
;   	  Inputs: DEHL = time to load (centiseconds)
; 	  Destroys: A,D,E,H,L,F
;
PTIME:	PUSH	BC
	PUSH	DE
	PUSH	HL
	CALL	TICKS
	LD	B,H
	LD	C,L
	POP	HL
	OR	A
	SBC	HL,BC
	LD	(OFFLO),HL
	LD	B,D
	LD	C,E
	POP	HL
	SBC	HL,BC
	LD	(OFFHI),HL
	POP	BC
	RET
;
; Get OS elapsed-time clock
;  Outputs: DEHL = time (centiseconds)
; Destroys: A,B,C,D,E,H,L,F
;
TICKS:	LD	C,248		;RunCPM-specific function call
	CALL	BDOS
	PUSH	DE
	EX	DE,HL
	OR	A
	SBC	HL,HL
	LD	BC,-5
	LD	A,32
DIV0:	ADD	HL,BC
	JR	C,DIV1
	SBC	HL,BC
DIV1:	RL	E
	RL	D
	EX	(SP),HL
	RL	L
	RL	H
	EX	(SP),HL
	ADC	HL,HL
	DEC	A
	JR	NZ,DIV0
	EX	DE,HL
	POP	DE
	RET
;
;INKEY	- Sample keyboard with specified wait.
;   	  Inputs: HL = Time to wait (centiseconds)
;  	  Outputs: Carry reset indicates time-out.
;                  If carry set, A = character typed.
; 	  Destroys: A,D,E,H,L,F
;
INKEY:	PUSH	BC
	PUSH	HL
	CALL	TICKS
	POP	DE
	ADD	HL,DE
WAIT:	PUSH	HL
	LD	C,6
	LD	E,0FFH
	CALL	BDOS
	POP	HL
	OR	A
	SCF
	JR	NZ,INKEY1
	PUSH	HL
	CALL	TICKS
	POP	DE
	SBC	HL,DE
	EX	DE,HL
	JR	C,WAIT	
INKEY1:	POP	BC
	RET
;
;CLS	- Clear screen.
;	  (Customise to suit your VDU)
; 	  Destroys: A,D,E,H,L,F
;
CLS:	RET
;
;PCSR	- Move cursor to specified position.
;   	  Inputs: DE = horizontal position (LHS=0)
;                 HL = vertical position (TOP=0)
; 	  Destroys: A,D,E,H,L,F
;
PCSR:	RET
;
;GCSR	- Return cursor coordinates.
;   	  Outputs:  DE = X coordinate (POS)
;                   HL = Y coordinate (VPOS)
;  	  Destroys: A,D,E,H,L,F
;
GCSR:	LD	DE,0
	LD	HL,0
	RET
;
;COUT - Output a character to the console
;   Inputs: A = character
; Destroys: A,F
;
COUT:	PUSH	BC
	PUSH	DE
	PUSH	HL
	LD	E,A
	LD	C,2
	CALL	BDOS
	POP	HL
	POP	DE
	POP	BC
	RET
;
;	DEFS 0$ > 1F0h
;    ; Código a ensamblar si la dirección actual es mayor que 1F0h
;	ERROR	"INSUFFICIENT SPACE"
;	ENDIF

;
;	ORG	1F0H
;
OFFLO:	DEFW	0		;TIME OFFSET LO
OFFHI:	DEFW	0		;TIME OFFSET HI
	DEFB	80		;WIDTH
	DEFB	'G' & 1FH	;CURSOR UP
	DEFB	'O' & 1FH	;CURSOR DOWN
	DEFB	'F' & 1FH	;START OF LINE
	DEFB	'N' & 1FH	;END OF LINE
	DEFB	'X' & 1FH	;DELETE TO END OF LINE
	DEFB	08H		;BACKSPACE & DELETE
	DEFB	'U' & 1FH	;DEL TO START OF LINE
	DEFB	'J' & 1FH	;CURSOR LEFT
	DEFB	'L' & 1FH	;CURSOR RIGHT
	DEFB	'R' & 1FH	;DELETE CHARACTER
	DEFB	'Q' & 1FH	;INS/OVR TOGGLE
;
FIN:	DEFS 	0
