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

;
	PUBLIC	CLRSCN
	PUBLIC	PUTCSR
	PUBLIC	GETCSR
	PUBLIC	PUTIME
	PUBLIC	GETIME
	PUBLIC	GETKEY
	PUBLIC	BYE
	PUBLIC  KEY_TAB_END
;	
	EXTERN COLD
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


CALSLT    EQU 	001CH
EXPTBL    EQU 	0FCC1H


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
; Destroys: A,D,E,H,L,F
;

TICKS: 					;MSX specific function call
    JIFFY   EQU 0FC9EH    
    LD  A, (JIFFY+0)    ; Byte menos significativo (Byte 0)
    LD  L, A
	LD  A, (JIFFY+1)    ; Byte 1
	LD  H, A
	XOR  E
	XOR  D
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
CLS: PUSH IX
	PUSH IY
    LD	IX,00C3H             ;address of BIOS routine
	LD     IY,(EXPTBL-1)       ;BIOS slot in iy
	XOR  A
	CALL   CALSLT              ;interslot call
	POP IY
	POP IX
	RET
;
;PCSR	- Move cursor to specified position.
;   	  Inputs: DE = horizontal position (LHS=0)
;                 HL = vertical position (TOP=0)
; 	  Destroys: A,D,E,H,L,F
;


PCSR: 
	PUSH IX
	PUSH IY
    LD  H, E       ; Cargar en C la coordenada X (columna)
    LD	IX,00C6H             ;address of BIOS routine
	LD     IY,(EXPTBL-1)       ;BIOS slot in iy
	CALL   CALSLT              ;interslot call
	POP IY
	POP IX
	RET
;
;GCSR	- Return cursor coordinates.
;   	  Outputs:  DE = X coordinate (POS)
;                   HL = Y coordinate (VPOS)
;  	  Destroys: A,D,E,H,L,F
;
GCSR:
	LD DE, (0F3DCH)
	LD HL, (0F3DDH)
	XOR D
	XOR H
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
	DEFB	1EH	;CURSOR UP
	DEFB	1FH	;CURSOR DOWN
	DEFB	0BH	;START OF LINE
	DEFB	0Eh	;END OF LINE
	DEFB	05H	;DELETE TO END OF LINE
	DEFB	08H		;BACKSPACE & DELETE
	DEFB	00H	;DEL TO START OF LINE
	DEFB	1DH	;CURSOR LEFT
	DEFB	1CH ;CURSOR RIGHT
	DEFB	7FH	;DELETE CHARACTER
	DEFB	12h	;INS/OVR TOGGLE

;	DEFB	'G' & 1FH	;CURSOR UP
;	DEFB	'O' & 1FH	;CURSOR DOWN
;	DEFB	'F' & 1FH	;START OF LINE
;	DEFB	'N' & 1FH	;END OF LINE
;	DEFB	'X' & 1FH	;DELETE TO END OF LINE
;	DEFB	08H		;BACKSPACE & DELETE
;	DEFB	'U' & 1FH	;DEL TO START OF LINE
;	DEFB	'J' & 1FH	;CURSOR LEFT
;	DEFB	'L' & 1FH	;CURSOR RIGHT
;	DEFB	'R' & 1FH	;DELETE CHARACTER
;	DEFB	'Q' & 1FH	;INS/OVR TOGGLE
;
KEY_TAB_END:	DEFS 	0
