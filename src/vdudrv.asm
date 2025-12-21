;	TITLE	COPYRIGHT (C) R.T.RUSSELL 1983-2024
;
;PATCH FOR BBC BASIC TO MSX-DOS
;* ACORN COMPUTERS Z80 TUBE VERSION  *
;(C) COPYRIGHT R.T.RUSSELL, 02-01-1984
;VERSION 5.0, 12-07-2024
;
OSWORD	EQU	0FFF1H
OSBYTE	EQU	0FFF4H
;
CPM	EQU	5

BDOS_GET_DATE    EQU 2AH 	;get date
BDOS_SET_DATE    EQU 2BH 	;set date
BDOS_GET_TIME    EQU 2CH 	;get time
BDOS_SET_TIME    EQU 2DH 	;set time
BDOS_CONSOLE_OUTPUT EQU 02H


BEL	EQU	07H
CUP	EQU	1EH	;CURSOR UP
CDOWN	EQU	1FH	;CURSOR DOWN
STLN	EQU	0BH	;START OF LINE
EDLN	EQU	0Eh	;END OF LINE
DENL	EQU	05H	;DELETE TO END OF LINE
BACK	EQU	08H		;BACKSPACE & DELETE
CR 	EQU 0DH
LF 	EQU 0AH
DBACK	EQU	00H	;DEL TO START OF LINE
CLF	EQU	1DH	;CURSOR LEFT
CRG	EQU	1CH ;CURSOR RIGHT
DEL	EQU	7FH	;DELETE CHARACTER
INS	EQU	12h	;INS/OVR TOGGLE


ESC	EQU	1BH
TBY	EQU	0FH
TTO	EQU	0B8H
TFILL	EQU	03H
;


;VDP

; --- Definiciones de Puertos y Registros VDP ---
VDP_DATA_PORT   EQU 98h
VDP_CTRL_PORT   EQU 99h

R36 EQU 36
R38 EQU 38
R40 EQU 40
R42 EQU 42
R44 EQU 44
R46 EQU 46   ; CMD register when using this command set

; --- Subrutina Principal ---
; --- Variables de Datos (en segmento de datos/BSS) ---
StartX:     DW 50   ;0
StartY:     DW 50   ;2
EndX:       DW 200  ;4
EndY:       DW 150  ;6
Maj:		DW 150  ;8
Min:		DW 100  ;10
lineFlags:	DB 0    ;12
Color:		DB 0    ;13
LogOp:		DB 0    ;14
vdp_cmd:		DB 0	;15




CPL_HL:
    ; Negar el byte bajo (L)
    ld a, l       ; Cargar el contenido de L en el acumulador A
    cpl           ; Complementar A bit a bit (A = ~A)
    ld l, a       ; Guardar el resultado negado de vuelta en L
    ; Negar el byte alto (H)
    ld a, h       ; Cargar el contenido de H en el acumulador A
    cpl           ; Complementar A bit a bit (A = ~A)
    ld h, a       ; Guardar el resultado negado de vuelta en H
	inc hl
    ret           ; Retornar de la subrutina (si se usa como CALL)
DRAW_LINE_CMD:
	push ix
	ld ix,StartX
	ld (ix+12),0 ;reset flags
	ld hl,(ix)
	ld de,(ix+4)
	or a
	set 2,(ix+12)
	SBC HL, DE
	bit 7,H
	jr Z,_positive_left_flag
	res 2,(ix+12)
	call CPL_HL    ;nbX es ahora positivo 
_positive_left_flag:
	ld (ix+8),hl
	ld hl,(ix+2)
	ld de,(ix+6)
	or a
	set 3,(ix+12)
	SBC HL, DE
	bit 7,H
	jr Z,_positive_up_flag
	res 3,(ix+12)
	call CPL_HL    ;nbY es ahora positivo 
_positive_up_flag:
	ld (ix+10),HL
	ld b,(ix+12)
	ld a,(ix+14)
;who is larger
	ld hl,(ix+8)
	ld de,(ix+10)
	or a
	SBC HL, DE
	bit 7,H
	jr Z, _hl_geq_de
	SET 0,(ix+12)
	ld hl,(ix+8)
	LD (ix+8),de
	LD (ix+10),hl

_hl_geq_de:
_iline:

	call	WAIT_VDP_READY
	ld	a,36
	out	(VDP_CTRL_PORT),a
	ld	a,128+17
	out	(VDP_CTRL_PORT),a	;R#17 := 36
	ld	c,09bh
	xor a
	ld hl,(ix)
	ld de,(ix+2)
	out	(c),l		;X from R#36
	out	(c),h		;       R#37
	out	(c),e		;Y from R#38
	out	(c),d		;       R#39
	ld hl,(ix+8)	;Maj    
	ld de,(ix+10)	;Min    
	out	(c),l		;Majsid R#40
	out	(c),h		;       R#41
	out	(c),e		;Minsid R#42
	out	(c),d       ;       R#43
	ld a,(ix+13)    ;Color
	out	(c),a		;       R#44   
	ld a,(ix+12)	;Flags
	out	(c),a       ;       R#45
	ld a,(ix+14)	;logop  
	ld l,(ix+15)	;vdp command
	or l
	out	(c),a		;       R#46
	pop ix
	ret


	;E is the physical color
	;and the index in the palette table
_ipalette:    
	;call	WAIT_VDP_READY i think we dont need that
	ld	a,E
	out	(VDP_CTRL_PORT),a
	ld	a,128+16
	out	(VDP_CTRL_PORT),a	   ;R#16 := pysical 
	ld	c,09bh
	xor a
	ld ix,PALETTE
	rl e
	ld d,0
	add ix,de
	ld a,(ix+1)
	out	(c),a
	ld a,(ix)      ;red and blue colors
	and 00001111b  ;cmd line
	out	(c),a      ;green color
	ret

_plotSinglePoint:
	push ix
	ld ix,StartX
	call	WAIT_VDP_READY
	ld	a,36
	out	(VDP_CTRL_PORT),a
	ld	a,128+17
	out	(VDP_CTRL_PORT),a	;R#17 := 36
	ld	c,09bh
	xor a
	ld hl,(ix+4)
	ld de,(ix+6)
	out	(c),l		;end X 
	out	(c),h
	out	(c),e		;end Y
	out	(c),d
	ld a,(ix+13)		;Color
	out	(VDP_CTRL_PORT),a
	ld	a,128+44
	out	(VDP_CTRL_PORT),a	        ;R#44 := Color
	ld a,(ix+14)		;logical op
	or	01010000b		;cmd pset
	out	(VDP_CTRL_PORT),a
	ld	a,128+46
	out	(VDP_CTRL_PORT),a	        ;R#44 := Color
	pop ix
	ret


;changes fore an back color (input aA)
_chColors:
    OUT  (VDP_CTRL_PORT), A
	LD   A, 128+7
    OUT  (VDP_CTRL_PORT), A
    RET

WAIT_VDP_READY:
	
    ld a,2
    di
    out (VDP_CTRL_PORT),a     ; select s#2
    ld a,15 + 128
    out (VDP_CTRL_PORT),a
    in a,(VDP_CTRL_PORT)
    rra
    ld a,0          ; back to s#0, enable ints
    out (VDP_CTRL_PORT),a
    ld a,15 + 128
    ei
    out (VDP_CTRL_PORT),a     ; loop if vdp not ready (CE)
    jp c,WAIT_VDP_READY
    ret

	EXTERN	ITEMI
	EXTERN	EXPRI
	EXTERN	COMMA
	EXTERN	TERMQ
	EXTERN	BRAKET
	EXTERN	EXTERR
	EXTERN	STOREN
	EXTERN	TRAP
	EXTERN	VAR
	EXTERN	NXT
	EXTERN	XEQ
;
	EXTERN	ACCS
	EXTERN	COUNT
	EXTERN	WIDTH
;
    EXTERN  FPP       ;for Math subrutines operation

; ---------MSX ROM BIOS SUBRUTINES
    EXPTBL      EQU 	0FCC1h      ;ROM BIOS SLOT
    CHGMOD      EQU     005Fh       ;change screen mode A
                                    ;Input    : A  - SCREEN mode 
    CALSLT      EQU     001Ch       ;Call inter-slot rom subrutine in IX
	CHGCLR      EQU		0062h		;change color
    GRPPRT      EQU     008Dh       ;Function : Displays a character on the graphic screen
                                    ;Input    : A  - ASCII value of the character to print 
    CHPUT       EQU     00A2h       ;Function : Displays one character
                                    ;Input    : A  - ASCII code of character to display
    CHGET       EQU     009Fh       ;Function : One character input (waiting)
                                    ;Output   : A  - ASCII code of the input character 
	POSIT		EQU  	00C6h		;Function: 	Moves the cursor
									;Input: 	H = X-coordinate of the cursor, L for the Y-coordinate
									;Output: 	None

	

; ---------MSX ROM BIOS VARS
	FORCLR 		EQU		0F3E9h 	 	;Foreground color
	BAKCLR 		EQU 	0F3EAh		;backgroud color
	BDRCLR 	 	EQU		0F3EBh 	 	;Border color
	CSRXTP		EQU 	0FCB5h		;X CURSOR position in text mode
	CSRYTP		EQU 	0FCB6h		;Y CURSOR position in text mode
	MAXCOL		EQU		0FCB1h		;MAX columns in text mode
	MAXROW		EQU 	0FCB0h		;MAX rows in text mode
    GXPOSH      EQU     0FCB3h 	 	;2 	X-position of graphic cursor
    GYPOSH		EQU     0FCB5h 	 	;2 	Y-position of graphic cursor
    GRPACX      EQU     0FCB7h 	 	;2 	X Graphics Accumulator
    GRPACY   	EQU     0FCB9h 	 	;2  Y Graphics Accumulator  
    SCRMOD      EQU     0FCAFh      ; Current Screen mode
	LINL40		EQU		0F3AEh
;----------Graphic Viewport variables----------
	VDU_GVOX:	DEFW 0000H	
	VDU_GVOY:	DEFW 0000H
	VDU_GVXW: 	DEFW 0000H
	VDU_GVXH:	DEFW 0000H
;----------Text Viewport variables-------------
	VDU_TVOX:	DEFB 0000H	
	VDU_TVOY:	DEFB 0000H
	VDU_TVXW: 	DEFB 0000H
	VDU_TVXH:	DEFB 0000H
;-----------Palete copy in ram-----------------
	PALETTE:  ;logical color,B,RG
	  			DB 00000000b,00000000b
				DB 00010010b,00000000b
				DB 00100000b,00000011b
				DB 00110010b,00000011b
				DB 01000000b,00011000b
				DB 01010010b,00011000b
				DB 01100000b,00011011b
				DB 01110010b,00011011b
				DB 10000010b,00100111b
				DB 10010111b,00000000b
				DB 10100000b,00000111b
				DB 10110111b,00000111b
				DB 11000000b,00111000b
				DB 11010111b,00111000b
				DB 11100000b,00111111b
				DB 11110111b,00111111b

;  A : logical color 
;  returns A: physical color
_findPhysicalColor:
	LD B,15
	RLA
	RLA
	RLA
	RLA
	AND 11110000B
	LD E,A
	LD HL, PALETTE
_fPC_loop:
	LD A,(HL)
	AND 11110000B
	CP  E
	jr Z,_fPC_exit_loop
	INC HL
	INC HL
	DJNZ _fPC_loop
_fPC_exit_loop:
	LD A,15
	SUB B
	RET

;----------------------------------------------
	SCROLL_Y_POS: DEFB 00H


;
	;PUBLIC	OSCALL      implemented in cmos.asm
	EXTERN	CLRSCN      ;implemented in dist.asm
	;PUBLIC	PUTCSR      implemented in dist.asm
	;PUBLIC	GETCSR      implemented in dist.asm
	;PUBLIC	PUTIME      implemented in dist.asm
	EXTERN	GETIME      ;implemented in dist.asm
	;PUBLIC	OSKEY       implemented in cmos.asm
;
	PUBLIC	CLG
	PUBLIC	MOVE
	PUBLIC	DRAW
	PUBLIC	PLOT
	PUBLIC	MODE
	PUBLIC	COLOUR
	PUBLIC	GCOL
	PUBLIC	ADVAL
	PUBLIC	SOUND
	PUBLIC	ENVEL
	PUBLIC	POINT
;
	PUBLIC	CIRCLE
	PUBLIC	ELLIPS
	PUBLIC	FILL
	PUBLIC	MOUSE
	PUBLIC	ORIGIN
	PUBLIC	RECTAN
	PUBLIC	LINE
	PUBLIC	TINT
	PUBLIC	WAIT
	PUBLIC	SYS
	PUBLIC	CSRON
	PUBLIC	CSROFF
;
	PUBLIC	PUTIMS
	PUBLIC	GETIMS
	PUBLIC	TINTFN
	PUBLIC	MODEFN
	PUBLIC	WIDFN
	PUBLIC  BDOS
	PUBLIC  BDOS0
	PUBLIC  OSWRCH_WITHOUT_READ
	PUBLIC  VDU_GVOX	
	PUBLIC  VDU_GVOY	
	PUBLIC  VDU_GVXW	
	PUBLIC  VDU_GVXH	
	PUBLIC  PCSR
;



SCRAP:	DEFS	31
	DEFB	0
;

;BDOS	- Save the IX & IY registers & before performing a
;	      msx-dos or CP/M function call in C.
;
;------------ VDU variables
VDU_MODE:  DEFB 0   ; VDU command in execution, 0 if no command is in execution
VDU_ARGV: DEFS	32  ; VDU Command Arguments vector 
VDU_ARGC: DEFB 0    ; VDU Command arguments counter
VDU_ARGC_LIST:      ; VDU number of arguments table
	DEFB    0 ;VDU 0 Does nothing.
	DEFB    0 ;VDU 1 provided the printer has been enabled (with VDU 2), tthe next character (byte) is sent to the printer and not to the screen.
	DEFB    0 ;VDU 2 enables the printer. It causes all subsequent output to be sent to both the screen and the printer. 
	DEFB    0 ;VDU 3 disables the printer. It cancels the effect of VDU 2 
	DEFB    0 ;VDU 4 causes text to be written at the text cursor position in the normal way.
	DEFB    0 ;VDU 5 causes text to be written at the graphics cursor position.
	DEFB    0 ;VDU 6 enables output to the VDU screen.
	DEFB    0 ;VDU 7 causes a short 'beep' from the speaker.
	DEFB    0 ;VDU 8 moves the text cursor one character to the left
	DEFB    0 ;VDU 9 moves the text cursor one character to the right. 
	DEFB    0 ;VDU 10 moves the text cursor down one line. 
	DEFB    0 ;VDU 11 moves the text cursor up one line. 
	DEFB    0 ;VDU 12 is identical to CLS
	DEFB    0 ;VDU 13 moves the text cursor to the left edge of the text window
	DEFB    0 ;VDU 14 enables auto-paging mode. 
	DEFB    0 ;VDU 15 disables auto-paging mode. 
	DEFB    0 ;VDU 16 is identical to CLG. 
	DEFB    1 ;VDU 17 is identical to COLOUR.N  text foreground (n<128) or background (n>=128) colours to the value n.
	DEFB    2 ;VDU 18 is identical to GCOL. k,c
	DEFB    5 ;VDU 19 The Palette, 1,p,r,g,b
	DEFB    0 ;VDU 20 Restore Default Colour Setting, COLOUR 7,COLOUR 128,GCOL 0,7,GCOL 0,128 and default palette
	DEFB    0 ;VDU 21 disables the VDU until a VDU 6 is received. 
	DEFB    1 ;VDU 22 is identical to MODE, except that MODE zeros the value of COUNT whereas VDU 22 does not.
	DEFB    1 ;VDU 23, Depends on next byte (mode)
	DEFB    4*2 ;VDU 24 In the graphics modes, VDU 24 defines a graphics window. 
	DEFB    1+(2*2) ;VDU 25 is identical to the PLOT command
	DEFB    0 ;VDU 26 resets the text and graphics windows to their default positions 
	DEFB    0 ;VDU 27 sends the next byte to the screen without interpreting it as a control character.
	DEFB    4 ;VDU 28 defines a text window. 
	DEFB    2*2 ;VDU 29 moves the graphics origin to the coordinates specified by the following two words (
	DEFB    0 ;VDU 30 homes the text cursor to the top left corner of the text window. In VDU 5 mode, VDU 30 homes the graphics cursor to the top left corner of the graphics window. 
	DEFB    2 ;VDU 31 is identical to PRINT TAB(x,y). It positions the text cursor according to the following two bytes. 
	DEFB    0 ;VDU 127 Delete the character to the left of the cursor and backspace the cursor and all the characters on the line to the right of the cursor.
VDU_SUBR_LIST:		; VDU jump table
	DEFW VDU0
	DEFW VDU1
	DEFW VDU2
	DEFW VDU3
	DEFW VDU4
	DEFW VDU5
	DEFW VDU6
	DEFW VDU7
	DEFW VDU8
	DEFW VDU9
	DEFW VDU10
	DEFW VDU11
	DEFW VDU12
	DEFW VDU13
	DEFW VDU14
	DEFW VDU15
	DEFW VDU16
	DEFW VDU17
	DEFW VDU18
	DEFW VDU19
	DEFW VDU20
	DEFW VDU21
	DEFW VDU22
	DEFW VDU23
	DEFW VDU24
	DEFW VDU25_DRV ;VDU25 is already defined by the original code for BBC MICRO 
	DEFW VDU26
	DEFW VDU27
	DEFW VDU28
	DEFW VDU29
	DEFW VDU30
	DEFW VDU31
	;DEFW VDU127


;...
BDOS0:	PUSH	BC
	PUSH	DE
	PUSH	HL
	EXX
	PUSH	BC
	PUSH	DE
	PUSH	HL
	EXX
	LD	C,A
	CALL	BDOS
	INC	H
	DEC	H
	EXX
	POP	HL
	POP	DE
	POP	BC
	EXX
	POP	HL
	POP	DE
	POP	BC
	RET

BDOS:	
	PUSH	IX
	PUSH	IY
	LD A,C    ;Is Write a char to the console char is E
	CP 6
	JR Z, VDU_CMD ;VDU WRITE OR READ
	CP 2
	JR Z, VDU_CMD_W ;VDU WRITE
BDOS_CALL:
	CALL	CPM
	POP	IY
	POP	IX
	RET
VDU_GWRITE:
	CALL	GOSWRCH
	POP	IY
	POP	IX
	RET
VDU_CMD:
	LD A,E          
	CP 0FFH         ; if the byte is FF it is requesting for a key input
	JR Z, BDOS_CALL
VDU_CMD_W:
	LD A,(VDU_ARGC)	; how many arguments are we waiting?
	CP 0			; if not 0 read this byte is an argument 
	JR NZ,VDU_READ_PARAMS_MODE
	LD A,E  
	CP 32           ; else if the byte is ge to 31 it is not a VDU cmd
	JR NC, VDU_GWRITE
	LD HL,VDU_MODE	; else it is a VDU command
	LD (HL),E       ; stores current command in VDU_MODE
	LD HL,VDU_ARGV  ;
	LD (HL),0		;resets arguments
	LD HL,VDU_ARGC_LIST
	LD E,A			;updates the expected arguments for this VDU command
	LD D,0 
	ADD HL,DE       ;VDU_ARGC+VDU_CMD contains the number of arguments
	LD A,(HL)
	LD (VDU_ARGC), A
	LD A,(VDU_ARGC)	; how many arguments are we waiting?
	CP 0			; if not 0 read this byte is an argument 
	JR NZ, END_VDU   ;Start CALLING THE VDP COMMAND
EXEC_VDU_CMD:
	LD A,(VDU_MODE)   ;command number
	LD HL,VDU_MODE
	LD (HL),0         ;reset command
	ADD A, A          ; Multiply index by 2. 
	LD DE, VDU_SUBR_LIST
    ADD DE, A
    LD A,(DE)   ;low byte of vdu cmd address
	LD L,A 
	INC DE
	LD A,(DE)   ;high byte if vdu address
	LD H,A
	LD BC,END_VDU
	PUSH BC         ;STORES PC AFTER JP IN THE PILE 
	JP (HL)         ;CALLS THE VDP COMMAND
END_VDU:
	POP	IY
	POP	IX
	RET
VDU_READ_PARAMS_MODE:
	LD A,(VDU_ARGC) 
	DEC A ;one less argument left
	LD HL,VDU_ARGV	;Loads argv vector address
	;reads the number of arguments         
	ADD HL, A		;get the last position of the stack of parameters
	LD (HL),E		;put the byte at the top of the parameter stack
	LD (VDU_ARGC),A ;updates the number of arguments 
	CP 0            ;command has no more arguments so we have to execute 
	JR NZ, END_VDU
	JR EXEC_VDU_CMD


;
;VDU 0 does nothing, reset the command arguments counter
VDU0:RET
VDU1:RET
VDU2:RET
VDU3:RET
VDU4:RET
VDU5:RET
VDU6:RET
VDU7:
	;VDU 7 command is the bell character so we only need to call BDOS end return
   	LD 	E, BEL ;BELL
	LD  C, BDOS_CONSOLE_OUTPUT
	LD  B, 0 
	CALL GOSWRCH     
	RET
VDU8:
	;VDU 8 command is the ursor left character so we only need to call BDOS end return
   	LD 	E, CLF ;cursor left
	LD  C, BDOS_CONSOLE_OUTPUT
	LD  B, 0 
	CALL GOSWRCH    
	RET
VDU9:
	;VDU 9 command is the Cursor right character so we only need to call BDOS end return
   	LD 	E, CRG; Cursor right
	LD  C, BDOS_CONSOLE_OUTPUT
	LD  B, 0 
	CALL GOSWRCH   
	RET
VDU10:
	;VDU 10 command is the LF character so we only need to call BDOS end return
   	LD 	E, LF ;LF
	LD  C, BDOS_CONSOLE_OUTPUT
	LD  B, 0 
	CALL GOSWRCH 
	RET
VDU11:
	;VDU 11 command is the cursor up character so we only need to call BDOS end return
   	LD 	E, CUP ;Cursor UP
	LD  C, BDOS_CONSOLE_OUTPUT
	LD  B, 0 
	CALL GOSWRCH     
	RET
VDU12:
	CALL CLRSCN
	RET
VDU13:
	;VDU 13, D command is the CR character so we only need to call BDOS end return
   	LD 	E, CR ;CR
	LD  C, BDOS_CONSOLE_OUTPUT
	CALL GOSWRCH    
	RET
VDU14:RET
VDU15:RET
VDU16:RET
VDU17:
	LD A,(VDU_ARGV)         ;Logical foreground color  byte
	AND 00001111B
	call _findPhysicalColor
	LD D, A         ;D holds the index (physical color)
	LD A,(VDU_ARGV)         ;Logical foreground color  byte
	AND 11110000B
	RRA
	RRA
	RRA
	RRA
	call _findPhysicalColor
	;A holds the index of (physical color)
	LD E,A
	LD  HL,FORCLR
	LD (HL),D
	LD HL,BAKCLR
	LD (HL),E
	LD A,D
	RLA 
	RLA
	RLA
	RLA
	OR E
	call _chColors
 	RET
VDU18:
	;Name 	Operation 							LO3 LO2 LO1 LO0
	;IMP 	DC=SC 								0 	0 	0 	0
	;AND 	SC*DC 								0 	0 	0 	1
	;OR 	SC+DC 								0 	0 	1 	0
	;EOR 	SC*DC+SC*DC 						0 	0 	1 	1
	;NOT 	DC=SC 								0 	1 	0 	0
	;--- 										0 	1 	0 	1
	;--- 										0 	1 	1 	0
	;--- 										0 	1 	1 	1
	;TIMP 	if SC=0 then DC=DC else DC=SC 		1 	0 	0 	0
	;TAND 	if SC=0 then DC=DC else SC*DC 		1 	0 	0 	1
	;TOR 	if SC=0 then DC=DC else SC+DC 		1 	0 	1 	0
	;TEOR 	if SC=0 then DC=DC else SC*DC+SC*DC 1 	0 	1 	1
	;TNOT 	if SC=0 then DC=DC else DC=SC 		1 	1 	0 	0
	;--- 										1 	1 	0 	1
	;--- 										1 	1 	1 	0
	;--- 										1 	1 	1 	1
	;Plot mode:
	;0	Plot	The specified color overwrites the existing color (default).
	;1	OR	The specified color is bitwise ORed with the color that is already there.
	;2	AND	The specified color is bitwise ANDed with the color that is already there.
	;3	EOR	The specified color is Exclusive-ORed (XORed) with the existing color. This mode is often used for animation as drawing the same shape twice restores the original background.
	;4	Invert	The existing color is inverted, and the colour parameter is ignored.
	LD 	A,(VDU_ARGV+1)       ;Logical operation
	Ld  IX,Color
	LD 	(IX+1),A
	LD	DE, SCRMOD   
	LD	A, (DE)           ;A = display mode
	CP	8
	JR	Z, COLOR_256 ;no palette in 256 mode
	LD A,(VDU_ARGV)          ;Logical foreground color  byte
	call _findPhysicalColor
	LD (IX),A
	RET
	COLOR_256:
	LD A,(VDU_ARGV)          ;Logical foreground color  byte
	LD (IX),A
 	RET
VDU19:
	LD A,(VDU_ARGV)         ;Blue color  byte
	LD H,A                 
	LD A,(VDU_ARGV+1)       ;Green Color
	LD L,A
	LD A,(VDU_ARGV+2)       ;Red Color
	RLA
	RLA
	RLA
	OR H					;H=RB L=G  
	LD A,(VDU_ARGV+3) 	    ;physical color
	LD E,A
	LD D,0h
	LD A,(VDU_ARGV+4) 		;logical color
	RLA
	RLA
	RLA
	RLA
	LD IX,PALETTE
	RL E   ;X BYTES
	ADD IX,DE
	RR E
	OR L	
	LD (IX),A			    ;loads logical color in the palete and G tree bits
	LD (IX+1),H				;loads the RB byte
	CALL _ipalette			;sets the RGB palete register R#E
	RET
VDU20:RET
VDU21:RET
VDU22: 
;VDU 22 is identical to MODE, except that MODE zeros the value of COUNT whereas VDU 22 does not. 
;The mode is set according to the value of the byte following the VDU 22 command. 
;The example below sets mode 3.
;   VDU 22,3


		LD A,(VDU_ARGV)
		;Reset ORIGIN
		LD     HL,0
		LD     (VDU_GVOX),HL
		LD     (VDU_GVOY),HL
		;SCALE VIEWPORT WIDTH AND HEIGHT DEPENDING ON SCREEN MODE
		CP	   0
		JR     Z, MODE_TEXT_1
		CP     1;
		JR     Z, MODE_TEXT_2
		CP     2
		JR     Z, SET_VIEWPORT_LR
		CP     6;
		JR     Z, SET_VIEWPORT_HR
		CP     7;
		JR     Z, SET_VIEWPORT_HR
		CP     9;
		JR     Z, MODE_GRAP_1   ;IT IS BE MODE 1



	SET_VIEWPORT_MR:
		LD     HL,256
		LD     (VDU_GVXW), HL
		LD     HL,212
		LD     (VDU_GVXH), HL
		JR CALL_CHMOD
	
	SET_VIEWPORT_LR:
		LD     HL,256
		LD     (VDU_GVXW), HL
		LD     HL,192
		LD     (VDU_GVXH), HL
		JR CALL_CHMOD

	SET_VIEWPORT_HR:
		LD     HL,512
		LD     (VDU_GVXW), HL
		LD     HL,212
		LD     (VDU_GVXH), HL
		JR CALL_CHMOD
	MODE_TEXT_1:
	    ; La variable de sistema LINLEN (#F3EBH) almacena el número de columnas actual.
        LD      HL,LINL40   ; Dirección de la variable de sistema LINLEN
        LD      (HL),40    ; Establecer el valor de 80 columnas
		jr CALL_CHMOD			
	MODE_TEXT_2:
	    ; La variable de sistema LINLEN (#F3EBH) almacena el número de columnas actual.
        LD      HL,LINL40   ; Dirección de la variable de sistema LINLEN
        LD      (HL),80    ; Establecer el valor de 80 columnas
		XOR A
		jr CALL_CHMOD
	MODE_GRAP_1:
	    ; La variable de sistema LINLEN (#F3EBH) almacena el número de columnas actual.
		AND 00000111b
        LD      HL,LINL40   ; Dirección de la variable de sistema LINLEN
        LD      (HL),32     ; Establecer el valor de 80 columnas
			
	CALL_CHMOD:
		LD     IY,(EXPTBL-1)       ;BIOS slot in iy
		LD     IX, CHGMOD    
		CALL   CALSLT
		RET

VDU23:RET
VDU24:RET
VDU25_DRV:

	;0 	Move relative to the last point.
	;1 	Draw a line, in the current graphics foreground colour, relative to the last point.
	;2 	Draw a line, in the logical inverse colour, relative to the last point.
	;3 	Draw a line, in the background colour, relative to the last point.
	;4 	Move to the absolute position X, Y.
	;5 	Draw a line, in the current foreground colour, to the absolute coordinates specified by X and Y.
	;6 	Draw a line, in the logical inverse colour, to the absolute coordinates specified by X and Y.
	;7 	Draw a line, in the current background colour, to the absolute coordinates specified by X and Y.
	LD A,(VDU_ARGV)
	LD H,A
	LD A,(VDU_ARGV+1)
	LD L,A                 ;HL HAS Y COORD
	LD A,(VDU_ARGV+2)
	LD D,A
	LD A,(VDU_ARGV+3)
	LD E,A                 ;DE HAS X COORD
	CALL    SCALE_GRAPHIC_POS  ;LOADS DE WITH X POS AND HL WITH Y POS
	;CHECK PLOT MODE
	LD A,(VDU_ARGV+4) 		;loads plot mode
	;CHECKS AND CALCULATES PLOT ABSOLUTE/RELATIVE
	BIT 2,A 
	JR NZ,PLOT_ABSOLUTE
;PLOT_RELATIVE:
		;SBC HL,BC           ;HL = Y COORDINATE - GVX HEIGHT SALE NEGATIVO
		LD BC,(GRPACY);	Y Graphics Accumulator
		ADD HL,BC
		LD IX, (GRPACX);
		ADD IX,DE
		PUSH IX
		POP DE
		JR DO_NOT_NEED_INVERT
PLOT_ABSOLUTE:
    LD 		A,(VDU_GVXH)					  
	DEC 	A                 
	SUB 	L				  
	LD 		L,A 		  ; HL contiene Y escalado e invertido, H siempre es 0
	LD A,(VDU_ARGV+4) 		;loads plot mode AGAIN
DO_NOT_NEED_INVERT:
	BIT 6,A 
	JR NZ,PLOT_POINT
PLOT_LINE:
	LD IY,vdp_cmd
	LD (IY),01110000b
	AND 00000011B
	CP 0 					;move relative command
	JR Z, MOVETO
	CP 1 					;draw line relative pos in foreground color
	JR Z, DRAW_LINE_FGC
	CP 2 					;draw line relative pos in inverse foreground color
	JR Z, DRAW_LINE_IFG
	CP 3 					;draw line relative pos in background color
	JR Z, DRAW_LINE_BGC
	
	;CMD IMPLEMENTATION
	MOVETO:
		LD BC, (GRPACX)
		LD (GXPOSH), BC
		LD BC, (GRPACY)    ;use GXPOSH to store last pos before the new pos...
		LD (GYPOSH), BC	   ;not sure if this system variable is used for this purpose
		;store graphic cursor pos in the grpac
		LD (GRPACY),HL
		LD (GRPACX),DE
		RET
	DRAW_LINE_FGC:
	DRAW_LINE_IFG:
	DRAW_LINE_BGC:
		LD BC, (GRPACX)
		LD (StartX), BC
		LD BC, (GRPACY)
		LD (StartY), BC
		LD (EndX),DE
		LD (EndY),HL
		CALL DRAW_LINE_CMD
		LD HL,(EndX)
		LD (GRPACX),HL    ;move to the last point
		LD HL,(EndY)
		LD (GRPACY),HL
		RET
PLOT_POINT:
	BIT 5,A 	;(BIT 6 AND 5 )
	JP NZ,PLOT_RECTANGLE
	BIT 4,A     ;(BIT 6 AND 4 )
	JR NZ,PLOT_TRIANGLE
	AND 00000011b
	CP 0 					;move relative command
	JR Z, MOVETO
	CP 1 					;draw line relative pos in foreground color
	JR Z, PLOT_POINT_FGC
	CP 2 					;draw line relative pos in inverse foreground color
	JR Z, PLOT_POINT_IFG
	CP 3 					;draw line relative pos in background color
	JR Z, PLOT_POINT_BGC

	
	;CMD IMPLEMENTATION
	PLOT_POINT_FGC:
	PLOT_POINT_IFG:
	PLOT_POINT_BGC:
		LD BC, (GRPACX)
		LD (StartX), BC
		LD BC, (GRPACY)
		LD (StartY), BC
		LD (EndX),DE
		LD (EndY),HL
		CALL _plotSinglePoint
	RET

PLOT_TRIANGLE:
	LD IY,vdp_cmd
	LD (IY),01110000b    ;line cmd
	AND 00000011b
	CP 0 					;move relative command
	JP Z, MOVETO
	CP 1 					;draw line relative pos in foreground color
	JR Z, PLOT_TRIANGLE_FGC
	CP 2 					;draw line relative pos in inverse foreground color
	JR Z, PLOT_TRIANGLE_IFG
	CP 3 					;draw line relative pos in background color
	JR Z, PLOT_TRIANGLE_BGC
	;CMD IMPLEMENTATION
	PLOT_TRIANGLE_FGC:
	PLOT_TRIANGLE_IFG:
	PLOT_TRIANGLE_BGC:
		LD BC, (GXPOSH)    ;desde 1er punto
		LD (StartX), BC
		LD BC, (GYPOSH)
		LD (StartY), BC
		LD (EndX),DE		;hasta tercero
		LD (EndY),HL
		CALL DRAW_LINE_CMD
		LD BC, (EndX)		;desde tercero
		LD (StartX), BC
		LD BC, (EndY)
		LD (StartY), BC
		LD BC,(GRPACX)      ;hasta segundo
		LD (EndX),BC
		LD BC,(GRPACY)
		LD (EndY),BC
		CALL DRAW_LINE_CMD
		LD BC, (EndX)		;desde segundo
		LD (StartX), BC	
		LD BC, (EndY)
		LD (StartY), BC
		LD BC,(GXPOSH)      ;hasta primero
		LD (EndX),BC
		LD BC,(GYPOSH)
		LD (EndY),BC
		CALL DRAW_LINE_CMD

		RET


PLOT_RECTANGLE:
	LD IY,vdp_cmd
	LD (IY),01110000b    ;line cmd
	AND 00000011b
	CP 0 					;move relative command
	JP Z, MOVETO
	CP 1 					;draw rectangle at pos in foreground color
	JR Z, PLOT_RECTANGLE_FGC
	CP 2 					;draw rectangle at pos in inverse foreground color
	JR Z, PLOT_RECTANGLE_IFG
	CP 3 					;draw rectangle at pos in background color
	JR Z, PLOT_RECTANGLE_BGC
	;CMD IMPLEMENTATION
	PLOT_RECTANGLE_FGC:
		LD IY,vdp_cmd
		LD (IY),10000000b    ;line cmd
		LD BC, (GRPACX)
		LD (StartX), BC
		LD BC, (GRPACY)
		LD (StartY), BC
		LD (EndX),DE
		LD (EndY),HL
		CALL DRAW_LINE_CMD
		RET
	PLOT_RECTANGLE_IFG:
	PLOT_RECTANGLE_BGC:	
		LD BC,(GRPACY)
		LD (StartY),BC
		LD (EndY),HL
		LD BC,(GRPACX)      ;DESDE PUNTO ANTERIOR
		LD (StartX),BC
		LD (EndX),BC	
		PUSH HL
		PUSH DE
		CALL DRAW_LINE_CMD
		POP DE
		POP HL
		LD (StartY),HL
		LD BC,(GRPACX) 
		LD (StartX),BC
		LD (EndX),DE		
		LD (EndY),HL
		PUSH HL
		PUSH DE
		CALL DRAW_LINE_CMD
		POP DE
		POP HL
		LD (StartY),HL
		LD (StartX),DE
		LD BC,(GRPACY)
		LD (EndY),BC
		LD (EndX),DE
		PUSH HL
		PUSH DE
		CALL DRAW_LINE_CMD
		POP DE
		POP HL
		LD BC,(GRPACY)
		LD (StartY),BC
		LD (StartX),DE
		LD (EndY),BC
		LD BC,(GRPACX)      ;HASTA PUNTO ANTERIOR
		LD (EndX),BC
		CALL DRAW_LINE_CMD
		RET



VDU26:RET
VDU27:RET
VDU28:RET
VDU29:
	;LOADS DE WITH X POS AND HL WITH Y POS
	LD A,(VDU_ARGV)
	LD H,A
	LD A,(VDU_ARGV+1)
	LD L,A                 ;HL HAS Y COORD
	LD A,(VDU_ARGV+2)
	LD D,A
	LD A,(VDU_ARGV+3)
	LD E,A                 ;DE HAS X COORD
	;SCALE
    CALL    SCALE_GRAPHIC_POS
    LD 		A,(VDU_GVXH)					  
	DEC 	A                 
	SUB 	L				  
	LD 		L,A 		  ; HL contiene Y escalado e invertido, H siempre es 0
	;LOADS SCALED COORD TO VDU_GVOX AND VDU_GVOY
	LD A,E
	LD (VDU_GVOX),A 		;LOW BYTE GRAPHIC ORIGIN X
	LD A,D
	LD (VDU_GVOX+1),A 	;HIGH BYTE GRAPHIC ORIGIN X
	LD A,L
	LD (VDU_GVOY),A		;LOW BYTE GRAPHIC ORIGIN Y
	LD A,H
	LD (VDU_GVOY+1),A	;HIGH BYTE GRAPHIC ORIGIN X
	RET
VDU30:RET
VDU31:
	LD A,(VDU_ARGV)
	LD L,A
	LD H,0
	LD A,(VDU_ARGV+1)
	LD E,A    
	LD D,0             
	CALL PCSR
	RET
VDU127:RET



OSWRCH_WITHOUT_READ:
;------We need this OSWRCH_WITHOUT_READ for VDU Command because 
;------cmos.OSWRCH (cmos.asm) uses 06h cpm call 
;------06h subrutine cannot send FF to the console output
;------and we need that to send logical coords to the VDU
;------vdudrv.OSWRCH uses 02H subrutine so only writes to console
OSWRCH:	 
	PUSH AF
	PUSH DE
	LD  E, A
	LD 	A ,02H
	CALL BDOS0	
	POP DE
	POP AF
	RET

;GOSWRCH function: Writes a character to the standard output
;		 Input: E is the character to print
;destroys DE, AF'
GOSWRCH:
	LD 	   A,E  ; FOR CONVENIENCE WITH CPM CALL 
	LD     DE, SCRMOD   
	EX     AF,AF'
	LD     A, (DE)      ; A = display mode
	CP     2
	JR     C,GOSWRCH_TEXMODE
	EX     AF,AF'
	LD     IX, GRPPRT
	LD     IY,(EXPTBL-1)       ;BIOS slot in iy
	CALL   CALSLT    
	LD A,(GRPACY)
	CP 208
	CALL NC,VSCROLL    
	JR EXIT_GOSWRCH
GOSWRCH_TEXMODE:
	EX     AF,AF'
	LD     E, A     ; print one char in text mode 
	;LD 	C,2 
	CALL   CPM        ; call interslot subrutine
EXIT_GOSWRCH:
	RET



WRITE_VDP:
	LD A,L
	DI
	OUT (VDP_CTRL_PORT),A
	LD A,H
	OUT (VDP_CTRL_PORT),A
	EI
	RET

VSCROLL:

	LD A,(SCROLL_Y_POS)
	ADD A,1
	LD (SCROLL_Y_POS),A
	LD  L,A
	LD  H,97H  ;R23 CON BIT 7 A 1 PARA ESCRIBIR
	CALL WRITE_VDP
	LD A,0
	LD (GRPACY), A
	RET


;----READY----
;GETIMS	- Read real-time clock as string.
;  	  Outputs:  TIME$ in string accumulator
;                   E = string length (25)
; 	  Destroys: A,B,C,D,E,H,L,F
;
;USES Function STR - convert numeric value to ASCII string.
;   Inputs: HLH'L'C = integer or floating-point number
;           DE = address at which to store string
;           IX = address of @% format control
;    LD	    A,37
;    CALL    FPP		  ;STR          ; HLH'L' 
;THIS IS A VERY LONG IMPLEMENTATION BECAUSE MSX BIOS HAS NOTHING 
;LIKE STRING REPRESENTATION ALMOST AS A BDOS SUBRUTINE (mybe basic rom has something closer)
DAY_OF_WEEK:	DEFM	"Sun.Mon.Tue.Wed.Thu.Fri.Sat." ; four chars per day
DAYFORMAT:	    DEFM	"@00" 
MONTHS:	DEFM	"Jan Feb Mar Apr May Jun Jul Ago Sep Oct Nov Dec" ; four chars per month
YEARFORMAT:	    DEFM	"@00"

GETIMS:	
	LD	HL,SCRAP
	LD	(HL),0
    LD	C,BDOS_GET_DATE
    CALL  BDOS
    ;HL register ⟵ year
    ;D register ⟵ month
    ;E register ⟵ day of month
    ;A register ⟵ day of week
    PUSH HL
    PUSH DE

    LD HL, DAY_OF_WEEK
    RLCA   ;*2
    RLCA   ;*4
    LD C,A
    LD B,0
    ADD HL,BC
    LD  DE, SCRAP
    LD A,(HL)           ;copy four chars (day) from DAY_OF_WEEK+A to SCRAP
    LD (DE),A
    INC HL
    INC DE
    LD A,(HL)
    LD (DE),A
    INC HL
    INC DE
    LD A,(HL)
    LD (DE),A
    INC HL
    INC DE
    LD A,(HL)
    LD (DE),A           ;now scrap is "Day."

    INC DE
    LD HL,SP
    LD A,(HL)
    CP 9
    JR NC, LENGTH2  ; Salta si A < 9 (Si Carry=1). Si NO salta, sabemos que A >= 9.
    LD A,'0'
    LD (DE),A           ;now scrap is "Day. 0"
    INC DE    
;
;   Convert day of month to format dd  
LENGTH2:
    EXX
    POP HL              ;DE (month,day of month) was in the pile so now H is month and L is day of month
    LD A,L
    PUSH HL
    LD H,0
    EXX
    PUSH DE             ;Store DE
    LD HL,0
    LD C,0
    ;DE points to scrap next address
    LD IX, DAYFORMAT   ;leading 0 dd format 
    LD	    A,37
    CALL    FPP		   ;STR          ; HLH'L'C stores the number (C=0 means number is Integer)
    POP DE
    INC DE 
    INC DE 
    LD A,' '
    LD (DE),A           ;now scrap is "Day.dd "
    INC DE
;now month
    POP HL              ; H is month and L is day of month
    LD A,H
    LD HL, MONTHS
    RLCA ;a*2
    RLCA ;a*4
    LD C,A
    LD B,0
    ADD HL,BC
    LD A,(HL)           ;copy four chars (day) from MONTHS+A to SCRAP
    LD (DE),A
    INC HL
    INC DE
    LD A,(HL)
    LD (DE),A
    INC HL
    INC DE
    LD A,(HL)
    LD (DE),A
    INC HL
    INC DE
    LD A,(HL)
    LD (DE),A           
    INC DE ;now scrap is "Day.dd Mon "
    
;now the year
    EXX
    POP HL              ;year was in the pile
    EXX
    LD HL,0
    LD C,0
    LD IX, YEARFORMAT   ;leading 0 dd format 
    LD	    A,37
    PUSH DE
    CALL    FPP		   ;STR          ; HLH'L'C stores the number (C=0 means number is Integer)
    POP DE
    INC DE
    INC DE
    INC DE
    INC DE
    LD A,','
    LD (DE),A           ;now scrap is "Day.dd Mon yyyy,"
  
; now the time hh:mm:ss----------------------------------------
    LD	C,BDOS_GET_TIME
    CALL  BDOS
    ;H register ⟵ hour
    ;L register ⟵ minute
    ;D register ⟵ second
    ;E register ⟵ 1/100 second
    PUSH DE
    PUSH HL

    LD DE, SCRAP + 17
    PUSH DE
    LD A,H
    CP 9
    JR NC, LENGTH21  ; Salta si A < 9 (Si Carry=1). Si NO salta, sabemos que A >= 9.
    ;   Convert h to format hh 
    POP DE
    LD A,'0'
    LD (DE),A         
    INC DE
    PUSH DE

LENGTH21:
    LD L,H 
    LD H,0
    EXX
    LD HL,0
    POP DE
    LD C,0
    LD IX, DAYFORMAT   ;leading 0 dd format 
    LD	    A,37
    CALL    FPP		   ;STR          ; HLH'L'C stores the number (C=0 means number is Integer)
    INC DE
    LD A,':'
    LD (DE),A
    
;-----mm----------------------------------------
    POP HL
    LD DE, SCRAP + 21
    PUSH DE
    LD A,L
    CP 9
    JR NC, LENGTH22  ; Salta si A < 9 (Si Carry=1). Si NO salta, sabemos que A >= 9.
    POP DE
    ;   Convert m to format mm 
    LD A,'0'
    LD (DE),A  
    INC DE   
    PUSH DE

LENGTH22:
    LD H,0
    EXX
    LD HL,0
    POP DE
    LD HL,0
    LD C,0
    LD IX, DAYFORMAT   ;leading 0 dd format 
    LD	    A,37
    CALL    FPP		   ;STR          ; HLH'L'C stores the number (C=0 means number is Integer)
    LD DE, SCRAP + 23
    LD A,':'
    LD (DE),A
;-----ss----------------------------------------
    POP HL ; hl is de (seconds and csec)
    LD DE, SCRAP + 24
    PUSH DE
    LD A,H
    CP 9
    JR NC, LENGTH23  ; Salta si A < 9 (Si Carry=1). Si NO salta, sabemos que A >= 9.
    ;   Convert s to format ss 
    POP DE
    LD A,'0'
    LD (DE),A         
    INC DE
    PUSH DE

LENGTH23:
    LD L,H
    LD H,0
    EXX
    LD HL,0
    POP DE
    LD C,0
    LD IX, DAYFORMAT   ;leading 0 dd format 
    LD	    A,37
    CALL    FPP		   ;STR          ; HLH'L'C stores the number (C=0 means number is Integer)

    LD DE, SCRAP + 26
    LD (DE),0
	LD	HL,SCRAP
	LD	DE,ACCS
	LD	A,(HL)
	CP	E
	RET	Z
	LD	BC,26
	LDIR
	RET

;
;PUTIMS	- Wtite real-time clock as string.
;  	  Inputs:   string in string accumulator
;                   E = string length
; 	  Destroys: A,B,C,D,E,H,L,F
;
PUTIMS:	LD	A,E		;Length
	CP	26
	RET	NC
	LD	B,0
	LD	C,A
	LD	DE,SCRAP+1
	LD	HL,ACCS
	LDIR
	LD	HL,SCRAP
	LD	(HL),A
	LD	A,15
	JP	OSWORD

;-----------------------------------------SCALING UTILITIES-------------------------------------------
; SCALE_POS convert row/Column to pixel position
; Inputs: H = Fila (0-23), L = Columna (0-79) (H = Y, L = X)
; Outputs:  DE = Coordenadas de píxel 
;          Los registros AF, DE se preservan
SCALE_TEXT_POS:

    ; get the screen mode
    LD A, (SCRMOD)      ; A = display mode

    ; calculates X (Columna * ancho_carácter)
    CP 2
    JR C, IS_TXT_MODE       ; Si A < 1 (Modos 0), es modo txt..ojo
    CP 6
    JR C, IS_32_COLUMNS     ; Si A < 6 (Modos 4 y 5), es de 40/32 columnas
    CP 8
    JR Z, IS_32_COLUMNS     ; Si A == 8, es de 32 columnas
    
    ; Lógica para 80 columnas (caracteres de 6 píxeles de ancho)
	CALL Y_CALC     ;E has Y pos
    LD A, L
    SLA A           ; A = Columna * 2
    ADD A, L        ; A = Columna * 3
    SLA A           ; A = Columna * 6  Carry bit one needed for High Res modes
	LD D, A         ; D = Coordenada X
	
    RET				; 

IS_32_COLUMNS:
    ; Lógica para 32 columnas (caracteres de 8 píxeles de ancho)
    LD A, L
    SLA A           ; A = Columna * 2
    SLA A           ; A = Columna * 4
    SLA A           ; A = Columna * 8
    LD D, A         ; D = Coordenada X


Y_CALC:
    ; Calcular Y (Fila * 8), siempre 8 píxeles de alto
    LD A, H
    SLA A           ; A = Fila * 2
    SLA A           ; A = Fila * 4
    SLA A           ; A = Fila * 8
    LD E, A         ; E = Coordenada Y
    RET


IS_TXT_MODE:
    ;HL has yx
    LD A, H     ; Guarda el contenido de H en A
    LD H, L     ; Mueve el contenido de L a H
    LD L, A     ; Mueve el contenido de A (que era H) a L
	INC H       ; IN MSX 1 IS POINT 0,0 IS 1,1
	INC L
    PUSH HL
    LD	IX, POSIT             ;address of posit BIOS routine
	LD     IY,(EXPTBL-1)       ;BIOS slot in iy
	CALL   CALSLT      ; CALSLT
    POP DE
    RET

; SCALE_GRAPHIC_POS:cALCULATES PIXEL COORDS
;   	  Inputs: DE = horizontal position (LEFT=0..1279)
;                 HL = vertical position (bottom=0..1023)
; 	  Destroys: D,E,H,L
;
SCALE_GRAPHIC_POS:
    PUSH AF                         ;STACK AF
    PUSH BC                         ;STACK AF,BC
    PUSH HL                         ;STACK AF,BC,HL

    ;ARITHMETIC & LOGICAL OPERATORS:
    ;All take two arguments, in HLH"L'C & DED'E"B.
    ; Output in HLH'L'C
    ; Subrutina para escalar un valor rango 1280 al ancho del viewport.
    ;

    LD      HL, (VDU_GVXW); Carga el primer entero en el registro HL

	BIT 	7,D
	JR      NZ,DE_IS_NEGATIVE
	EXX
	LD      DE, 0000H  
	JR 		DE_IS_POSITIVE ; Pone a cero DE (bits 31 al 16 del nº entero de 32 bits)
                 ; Cambia a los registros alternos HL' tiene el ancho del viewport     
                          ; DE' contiene la porsicion x
DE_IS_NEGATIVE:
    EXX  
	LD 		DE, 0FFFFH
DE_IS_POSITIVE:	
	;si es negativo debería poner a  ffff los bytes altos!
	;LD 		DE,FFFFh

	LD      HL, 0000H         ; Pone a cero HL (bits 31 al 16 del nº entero de 32 bits)
    LD      BC,0          ; exponente tiene que ser 0 en ambos numeros	
    LD	    A,10
    CALL    FPP		  ;MULTIPLY          ; HLH'L' contiene la multiplicacion

DIVIDE_BY_1280:
    EXX     
    LD      DE, 1280
    EXX
    LD      DE, 0         ; Pone a cero DE (bits 31 al 16 del nº entero de 32 bits)
                          ; Divisor está en DED'É'
    LD      BC,0          ; exponente tiene que ser 0 en ambos numeros
    LD	    A,1
    CALL    FPP		  ;IBDIV         ; after the mul and div the number is 16 bits
    EXX
    LD      DE,HL         ; carga H'L' en DE temporalmente
   

SCALE_1024_TO_VIEPORT_HEIGHT:
    POP     HL            ; HL es la coordenada y de nuevo
    PUSH    DE                  
    LD      DE, (VDU_GVXH); Carga el alto del view port
	BIT 	7,H
	JR      NZ,HL_IS_NEGATIVE
	EXX
	LD      HL, 0000H  
	JR 		HL_IS_POSITIVE ; Pone a cero DE (bits 31 al 16 del nº entero de 32 bits)
HL_IS_NEGATIVE:
    EXX
	LD 		HL, 0FFFFH
HL_IS_POSITIVE:	
    LD      DE,0
    LD	    A,10
    CALL    FPP		      ;MULTIPLY

DIVIDE_BY_1024:
    EXX
    LD      DE, 1024
    EXX
    LD      DE, 0         ; Divisor está en DED'É'
    LD      BC,0          ;exponente tiene que ser 0 en ambos numeros
    LD      A,1
    CALL    FPP		      ;IBDIV  after the mul and div the number is 16 bits
    EXX
    POP     DE            ; carga X escalado 
    POP     BC
    POP     AF
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
	LD A,E
	LD (CSRXTP),A
	LD A,L	
	LD (CSRYTP),A
	LD H,A
	LD L,E
	CALL SCALE_TEXT_POS ; DE has now scaled to graphic position;
						; C=1 for X>256
	LD  HL, GRPACY+1
	LD (HL), 0  ;accumulator Y coordenadas gráficas H
	dec HL
	LD (HL), E  ;accumulator Y coordenadas gráficas L
	dec HL
	JR NC, PCSR_NO_CARRY
	LD (HL), 1  ;accumulator X coordenada grafica D
	JR PCSR_CARRY
PCSR_NO_CARRY:
	LD (HL), 0  ;accumulator X coordenada grafica D
PCSR_CARRY:
	dec HL
	LD (HL), D  ;accumulator X coordenadas gráficas E
	
	dec  HL     ;
	LD (HL), 0  ; 0fCB6   ;no se para que sirve
	dec HL
	LD (HL), 0  ; 0fCB5   ;no se para que sirve
	dec  HL     ;
	LD (HL), 0  ; 0fCB4   ;no se para que sirve
	dec HL
	LD (HL), 0  ; 0fCB3   ;no se para que sirve	
	POP IY
	POP IX
	RET
	



;--------------------------------------VDU OPERATIONS--------------------------------------

;POINT - var=POINT(x,y)
; read the color of the pixel xy
POINT:	CALL	EXPRI
	EXX
	PUSH	HL
	CALL	CEXPRI
	EXX
	POP	DE
	CALL	BRAKET
	LD	IX,SCRAP
	LD	(IX+0),E
	LD	(IX+1),D
	LD	(IX+2),L
	LD	(IX+3),H
	LD	HL,SCRAP
	LD	A,9
	CALL	OSWORD
	LD	A,(IX+4)
	LD	L,A
	ADD	A,1
	SBC	A,A
	LD	H,A
RETEXX:	EXX
	LD	H,A
	LD	L,A
	XOR	A
	LD	C,A
	RET
;
;ADVAL - var=ADVAL(n)
;lectura de canales analogicos (joysticks p.e.) >0, botones de joystick =0 estado buffers <0
ADVAL:	CALL	ITEMI
	EXX
	LD	A,128
	CALL	OSBYTE
	XOR	A
	JR	RETEXX
;
;MODEFN - var=MODE
;----READY----
MODEFN:	    
	
	; get the screen mode
    LD  A, (SCRMOD)      ; A = display mode
	LD  L,A

RETU8:	XOR	A
	LD	H,A
	JR	RETEXX
;
;WIDFN - var=WIDTH
;----READY----
WIDFN:	LD	A,(WIDTH)
	LD	L,A
	JR	RETU8
;
;ENVEL - ENVELOPE var,var,var,var,var,var,var,
;                 var,var,var,var,var,var,var
;
ENVEL:	LD	B,0
	LD	IX,SCRAP
	PUSH	BC
	PUSH	IX
ENVEL1:	CALL	EXPRI
	EXX
	POP	IX
	POP	BC
	LD	(IX),L
	LD	A,B
	CP	13
	JR	Z,ENVEL2
	INC	B
	INC	IX
	PUSH	BC
	PUSH	IX
	CALL	COMMA
	JR	ENVEL1
ENVEL2:	LD	HL,SCRAP
	LD	A,8
	CALL	OSWORD
	JP	XEQ
;
;SOUND - SOUND var,var,var,var
;
SOUND:	LD	B,0
	LD	IX,SCRAP
	PUSH	BC
	PUSH	IX
SOUND1:	CALL	EXPRI
	EXX
	POP	IX
	POP	BC
	LD	(IX+0),L
	LD	(IX+1),H
	INC	IX
	INC	IX
	INC	B
	INC	B
	LD	A,B
	CP	8
	JR	Z,SOUND2
	PUSH	BC
	PUSH	IX
	CALL	COMMA
	JR	SOUND1
SOUND2:	LD	HL,SCRAP
	LD	A,7
	CALL	OSWORD
	JP	XEQ
;
;MODE - MODE n
;
MODE:	CALL	EXPRI
	XOR	A
	LD	(COUNT),A
	EXX
	LD	H,L
	LD	L,22        ;22 is the VDU command for changing screen mode
	CALL	WRCH2   ;writes the vdu command (22) and then the mode, changes the graphic mode
	JR	XEQGO1
;
;CLG
;
CLG:	LD	A,16
	CALL	OSWRCH
	JR	XEQGO1
;
;ORIGIN x,y
;
ORIGIN: CALL    EXPRI
        EXX
	PUSH	HL
        CALL    CEXPRI
	EXX
        POP	DE
	LD	C,29
	CALL	WRCH5
        JR	XEQGO1
;
;COLOUR n
;COLOUR n,p
;COLOUR n,r,g,b
;
COLOUR:	CALL	EXPRI		;n
	EXX
	LD	A,(IY)
	CP	','
    JR      Z,PALCOL
	LD	H,L
	LD	L,17
	CALL	WRCH2
	JR	XEQGO1
;
PALCOL:	PUSH	HL
	CALL	CEXPRI		;p or r
	EXX
	EX	DE,HL
	LD	HL,0
	LD	A,(IY)
	CP	','
	JR	NZ,PALET1
	PUSH	DE
	CALL	CEXPRI		;g
	EXX
	PUSH	HL
	CALL	CEXPRI		;b
	EXX
	POP	DE
	POP	BC
	LD	A,L
	POP	HL
	LD	D,C		;r
	LD	C,L		;n
	LD	L,E		;g
	LD	H,A		;b
	LD	E,16
	PUSH	BC
PALET1:	POP	BC
	LD	B,19
	CALL	WRCH6
	JR	XEQGO1
;
;GCOL [a,]b
;
GCOL:	CALL	EXPRI
	EXX
	LD	E,0
	LD	A,(IY)
	CP	','
	JR	NZ,GCOL0
	PUSH	HL
	CALL	CEXPRI
	EXX
	POP	DE
GCOL0:	LD	H,L
	LD	L,E
	LD	D,18
	CALL	WRCH3		;DLH
XEQGO1:	JP	XEQ
;
;CSRON  - Turn caret on
;CSROFF - Turn caret off
;
CSRON:	LD	C,1
	JR	CSRGO
;
CSROFF:	LD	C,0
CSRGO:	LD	A,23
	CALL	OSWRCH
	LD	A,1
	CALL	OSWRCH
	LD	A,C
	LD	B,8
CSRGO1:	CALL	OSWRCH
	XOR	A
	DJNZ	CSRGO1
	JR	XEQGO1
;
;LINE x1,y1,x2,y2
;
LINE:	CALL	EXPRI
	EXX
	PUSH	HL
	CALL	EXPR3
	EX	(SP),HL		;HL <- x1, (SP) <- y2
	PUSH	BC
	EX	DE,HL
	LD	C,4
	CALL	VDU25
	POP	DE
	POP	HL
	LD	C,5
	JR	PLOT4A
;
;CIRCLE [FILL] x,y,r
;
CIRCLE:	CP	TFILL
	PUSH	AF
	JR	NZ,CIRCL0
	INC	IY
CIRCL0:	CALL	EXPRI
	EXX
	PUSH	HL
	CALL	CEXPRI
	EXX
	PUSH	HL
	CALL	CEXPRI
	EXX
	POP	BC		;y
	POP	DE		;x
	PUSH	HL
	LD	L,C
	LD	H,B
	LD	C,4		; PLOT 4 = MOVE
	CALL	VDU25
	POP	DE		;r
	LD	HL,0
        POP	AF
	LD	C,145		; PLOT 145 = outline circle
	JR	NZ,PLOT4A
	LD	C,153		; PLOT 153 = filled circle
PLOT4A:	JR	PLOT4
;
;ELLIPSE [FILL] x,y,a,b
;
ELLIPS:	CP	TFILL
	PUSH	AF
	JR	NZ,ELLIP0
	INC	IY
ELLIP0:	CALL	EXPRI
	EXX
	PUSH	HL
	CALL	EXPR3
	EX	(SP),HL		;HL <- x, (SP) <- b
	PUSH	BC
	EX	DE,HL
	LD	C,4		; PLOT 4 = Move absolute
	CALL	VDU25
	POP	DE		;a
	PUSH	DE
	LD	HL,0
	LD	C,L		; PLOT 0 - Move relative
	CALL	VDU25
        POP	DE		;a
	XOR	A
	LD	L,A
	LD	H,A
	SBC	HL,DE
	EX	DE,HL
	POP	HL		;b
	POP	AF
	LD	C,193		; PLOT 193 = outline ellipse
	JR	NZ,PLOT4
	LD	C,201		; PLOT 201 = filled ellipse
	JR	PLOT4
;
;MOVE [BY} x,y
;DRAW [BY] x,y
;PLOT [BY] [n,]x,y
;FILL [BY] x,y
;
MOVE:	LD	C,4
	JR	PLOT1
;
DRAW:	LD	C,5
	JR	PLOT1
;
FILL:	LD	C,133
	JR	PLOT1
;
PLOT:	LD	C,69
	CP	TBY
	JR	Z,PLOT1
	CALL	EXPRI
	EXX
	PUSH	HL
	CALL	CEXPRI
	EXX
	LD	A,(IY)
	CP	','
	JR	Z,PLOT3
	POP	DE
	LD	C,69
	JR	PLOT4
;
PLOT1:	CP	TBY
	JR	NZ,PLOT2
	INC	IY
	RES	2,C		;Change absolute to relative
PLOT2:	PUSH	BC
	CALL	EXPRI
	EXX
PLOT3:	PUSH	HL
	CALL	CEXPRI
	EXX
	POP	DE
	POP	BC
PLOT4:	CALL	VDU25
	JP	XEQ
;
;RECTANGLE [FILL] x,y,w[,h] [TO xnew,ynew]
;
RECTAN:	CP	TFILL
	PUSH	AF
	JR	NZ,RECT0
	INC	IY
RECT0:	CALL	EXPRI
	EXX
	PUSH	HL
	CALL	CEXPRI
	EXX
	PUSH	HL
	CALL	CEXPRI
	EXX
	PUSH	HL
	LD	A,(IY)
	CP	','
	JR	NZ,RECT1
	CALL	CEXPRI
	EXX
RECT1:	POP	BC		;w
	POP	DE		;y
	EX	(SP),HL		;HL <- x, (SP) <- h
	PUSH	BC
	EX	DE,HL
	LD	C,4
	CALL	VDU25
	LD	A,(IY)
	CP	TTO
	JR	Z,RECTTO
	POP	DE		;w
	POP	HL		;h
	POP	AF
	JR	NZ,OUTLIN
	LD	C,97
	JR	PLOT4
;
;Block copy / move:
;
RECTTO:	INC	IY		; Bump over TO
	CALL	EXPRI
	EXX
	PUSH	HL
	CALL	CEXPRI
	EXX
	POP	BC		;newx
	POP	DE		;w
	EX	(SP),HL		;HL <- h, (SP) <- newy
	PUSH	BC
	LD	C,0
	CALL	VDU25
	POP	DE		;newx
	POP	HL		;newy
	POP	AF
	LD	C,190		; PLOT 190 - Block copy
	JR	NZ,PLOT4B
	DEC	C		; PLOT 189 - Block move
PLOT4B:	JR	PLOT4
;
;Outline rectangle:
;
OUTLIN:	LD	C,9		; PLOT 9 - draw relative
	PUSH	HL
	LD	HL,0
	CALL	VDU25		; side 1
	POP	HL
	PUSH	DE
	LD	DE,0
	CALL	VDU25		; side 2
	POP	DE
	PUSH	HL
	XOR	A
	LD	L,A
	LD	H,A
	SBC	HL,DE
	EX	DE,HL
	LD	L,A
	LD	H,A
	CALL 	VDU25		; side 3
	POP	DE
	XOR	A
	LD	L,A
	LD	H,A
	SBC	HL,DE
	LD	E,A
	LD	D,A
	JR	PLOT4B
;
;MOUSE x, y, b
;
MOUSE:	LD	A,128
	LD	HL,9
	CALL	OSBYTE
	PUSH	HL
	LD	A,128
	LD	HL,8
	CALL	OSBYTE
	PUSH	HL
	LD	A,128
	LD	HL,7
	CALL	OSBYTE
	PUSH	HL
	CALL	VAR
	POP	HL
	CALL	STOREI
	CALL	COMMA
	CALL	NXT
	CALL	VAR
	POP	HL
	CALL	STOREI
	CALL	COMMA
	CALL	NXT
	CALL	VAR
	POP	HL
	CALL	STOREI
XEQGO2:	JP	XEQ
;
;WAIT [n]
;
WAIT:	CALL	TERMQ
	JR	Z,XEQGO2
	CALL	EXPRI
	EXX
	LD	B,H
	LD	C,L
	CALL	GETIME
	ADD	HL,BC
	LD	BC,0
	EX	DE,HL
	ADC	HL,BC
	EX	DE,HL
WAIT1:	CALL	TRAP
	PUSH	DE
	PUSH	HL
	CALL	GETIME
	POP	BC
	OR	A
	SBC	HL,BC
	LD	H,B
	LD	L,C
	EX	DE,HL
	POP	BC
	SBC	HL,BC
	JR	NC,XEQGO2
	EX	DE,HL
	LD	D,B
	LD	E,C
	JR	WAIT1


VDU25:	LD	B,25
	WRCH6:	LD	A,B
		CALL	OSWRCH
	WRCH5:	LD	A,C
		CALL	OSWRCH
	WRCH4:	LD	A,E
		CALL	OSWRCH
	WRCH3:	LD	A,D
		CALL	OSWRCH
	WRCH2:	LD	A,L   
		CALL	OSWRCH
		LD	A,H
		JP	OSWRCH
	;
	EXPR3:	CALL	CEXPRI
		EXX
		PUSH	HL
		CALL	CEXPRI
		EXX
		PUSH	HL
		CALL	CEXPRI
		EXX
		POP	BC		;x2
		POP	DE		;y1
		RET
;
CEXPRI:	CALL	COMMA
	JP	EXPRI
;
STOREI:	BIT	7,A
	JR	NZ,EEK
	BIT	6,A
	JR	NZ,EEK
	EXX
	LD	HL,0
	LD	C,L
	JP	STOREN
;
EEK:	LD	A,50
	CALL	EXTERR
	DEFB	13H		;'Bad '
	DEFB	04H		;'MOUSE'
	DEFB	20H
	DEFB	15H		;'variable'
	DEFB	0
;
TINT:
TINTFN:
SYS:
	XOR	A
	CALL	EXTERR
	DEFM	"Sorry"
	DEFB	0
;

                                                          