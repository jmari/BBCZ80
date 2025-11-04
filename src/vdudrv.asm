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
OS_CONSOLE_OUTPUT EQU 02H


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
    EXTERN  OSWRCH

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
	EXTERN	SCRAP
;

    EXTERN    GOX	    ;GRAPHIC VIEWPORT ORIGIN  X in px
    EXTERN    GOY	    ;GRAPHIC VIEWPORT ORIGIN  Y in px
    EXTERN    GXW	    ;GRAPHIC VIEWPORT X WIDE in px
    EXTERN    GYH	    ;GRAPHIC VIEWPORT Y HEIGHT in px
    EXTERN    FPP       ;for Math subrutines operation

; ROM BIOS SUBRUTINES
    EXPTBL      EQU 	0FCC1H      ;ROM BIOS SLOT
    CHGMOD      EQU     005Fh       ;change screen mode A
                                    ;Input    : A  - SCREEN mode 
    CALSLT      EQU     001Ch       ;Call inter-slot rom subrutine in IX
    GRPPRT      EQU     008Dh       ;Function : Displays a character on the graphic screen
                                    ;Input    : A  - ASCII value of the character to print 
    CHPUT       EQU     00A2h       ;Function : Displays one character
                                    ;Input    : A  - ASCII code of character to display
    CHGET       EQU     009Fh       ;Function : One character input (waiting)
                                    ;Output   : A  - ASCII code of the input character 
; ROM BIOS VARS
    GXPOSH      EQU     0FCB3h 	 	;2 	X-position of graphic cursor
    GYPOSH      EQU     0FCB5h 	 	;2 	Y-position of graphic cursor
    GRPACXH     EQU     0FCB7h 	 	;2 	X Graphics Accumulator
    GRPACYH     EQU     0FCB9h 	 	;H  Y Graphics Accumulator  	
    GRPACYL     EQU     0FCBAh      ;L  Y Graphics Accumulator  
    SCRMOD      EQU     0FCAFh      ; Current Screen mode
;
	;GLOBAL	OSCALL      implemented in cmos.asm
	EXTERN	CLRSCN      ;implemented in dist.asm
	;GLOBAL	PUTCSR      implemented in dist.asm
	;GLOBAL	GETCSR      implemented in dist.asm
	;GLOBAL	PUTIME      implemented in dist.asm
	EXTERN	GETIME      ;implemented in dist.asm
	;GLOBAL	OSKEY       implemented in dist.asm
;
	GLOBAL	CLG
	GLOBAL	MOVE
	GLOBAL	DRAW
	GLOBAL	PLOT
	GLOBAL	MODE
	GLOBAL	COLOUR
	GLOBAL	GCOL
	GLOBAL	ADVAL
	GLOBAL	SOUND
	GLOBAL	ENVEL
	GLOBAL	POINT
;
	GLOBAL	CIRCLE
	GLOBAL	ELLIPS
	GLOBAL	FILL
	GLOBAL	MOUSE
	GLOBAL	ORIGIN
	GLOBAL	RECTAN
	GLOBAL	LINE
	GLOBAL	TINT
	GLOBAL	WAIT
	GLOBAL	SYS
	GLOBAL	CSRON
	GLOBAL	CSROFF
;
	GLOBAL	PUTIMS
	GLOBAL	GETIMS
	GLOBAL	TINTFN
	GLOBAL	MODEFN
	GLOBAL	WIDFN
	GLOBAL  BDOS
	GLOBAL  BDOS0
;



SCRAP:	DEFS	31
	DEFB	0
;

;BDOS	- Save the IX & IY registers & before performing a
;	      msx-dos or CP/M function call in C.
;
;vdu variables
VDU_MODE:  DEFB 0
VDU_ARGV: DEFS	32
VDU_ARGC: DEFB 0
VDU_ARGC_LIST:   DEFB    0 ;VDU 0 Does nothing.
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
			DEFB    0 ;VDU 22 is identical to MODE, except that MODE zeros the value of COUNT whereas VDU 22 does not.
			DEFB    1 ;VDU 23, Depends on next byte (mode)
			DEFB    4*2 ;VDU 24 In the graphics modes, VDU 24 defines a graphics window. 
			DEFB    3*2 ;VDU 25 is identical to the PLOT command
			DEFB    0 ;VDU 26 resets the text and graphics windows to their default positions 
			DEFB    0 ;VDU 27 sends the next byte to the screen without interpreting it as a control character.
			DEFB    4 ;VDU 28 defines a text window. 
			DEFB    2*2 ;VDU 29 moves the graphics origin to the coordinates specified by the following two words (
			DEFB    0 ;VDU 30 homes the text cursor to the top left corner of the text window. In VDU 5 mode, VDU 30 homes the graphics cursor to the top left corner of the graphics window. 
			DEFB    2 ;VDU 31 is identical to PRINT TAB(x,y). It positions the text cursor according to the following two bytes. 
			DEFB    0 ;VDU 127 Delete the character to the left of the cursor and backspace the cursor and all the characters on the line to the right of the cursor.
VDU_SUBR_LIST:
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
	DEFW VDU25
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
	LD	C,A
	CALL	BDOS
	INC	H
	DEC	H
	POP	HL
	POP	DE
	POP	BC
	RET

BDOS:	
	PUSH	IX
	PUSH	IY
	LD A,C    ;Is Write a char to the console char is E
	CP 6
	JR Z, VDU_CMD
BDOS_CALL:
	CALL	CPM
	POP	IY
	POP	IX
	RET
VDU_CMD:
	LD A,(VDU_ARGC)	; how many arguments are we waiting?
	CP 0			; if not 0 read this byte is an argument 
	JR NZ,VDU_READ_PARAMS_MODE
	LD A,E          
	CP 0FFH         ; else if the byte is FF it is requesting for a key input
	JR Z, BDOS_CALL
	CP 31           ; else if the byte is ge to 31 it is not a VDU cmd
	JR NC, BDOS_CALL
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
    LD A,(DE)
	LD L,A
	INC DE
	LD A,(DE)
	LD H,A
	LD BC,END_VDU
	PUSH BC         ;STORES PC AFTER JP IN THE PILE 
	JP (HL)         ;CALLS THE VDP COMMAND
END_VDU:
	POP	IY
	POP	IX
	RET
VDU_READ_PARAMS_MODE:
	LD HL,VDU_ARGV	;Loads argv vector address
	LD A,D          ;loads the byte parameter in A
	LD DE,(VDU_ARGC) ;reads the number of arguments
	LD D,0          
	ADD HL,DE		;get the last position of the stack of parameters
	LD (HL),A		;put the byte at the top of the stack
	DEC E			;one less argument left
	LD A,E
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
	;VDU 8 command is the Cursor left character so we only need to call BDOS end return
   	LD 	E, BEL ;LF
	LD  C, OS_CONSOLE_OUTPUT
	LD  B, 0 
	CALL  CPM       
	RET
VDU8:
	;VDU 8 command is the Cursor left character so we only need to call BDOS end return
   	LD 	E, CLF ;LF
	LD  C, OS_CONSOLE_OUTPUT
	LD  B, 0 
	CALL  CPM       
	RET
VDU9:
	;VDU 8 command is the Cursor left character so we only need to call BDOS end return
   	LD 	E, CRG;LF
	LD  C, OS_CONSOLE_OUTPUT
	LD  B, 0 
	CALL  CPM       
	RET
VDU10:
	;VDU 10 command is the LF character so we only need to call BDOS end return
   	LD 	E, LF ;LF
	LD  C, OS_CONSOLE_OUTPUT
	LD  B, 0 
	CALL  CPM       
	RET
VDU11:
	;VDU 10 command is the LF character so we only need to call BDOS end return
   	LD 	E, CUP ;LF
	LD  C, OS_CONSOLE_OUTPUT
	LD  B, 0 
	CALL  CPM       
	RET
VDU12:
	CALL CLRSCN
	RET
VDU13:
	;VDU 13, D command is the CR character so we only need to call BDOS end return
   	LD 	E, CR ;CR
	LD  C, OS_CONSOLE_OUTPUT
	LD  B, 0 
	CALL  CPM       
	RET
VDU14:RET
VDU15:RET
VDU16:RET
VDU17:RET
VDU18:RET
VDU19:RET
VDU20:RET
VDU21:RET
VDU22:RET
VDU23:RET
VDU24:RET
;VDU25:RET
VDU26:RET
VDU27:RET
VDU28:RET
VDU29:RET
VDU30:RET
VDU31:RET
VDU127:RET


BDOS_GET_DATE    EQU 2AH 	;get date
BDOS_SET_DATE    EQU 2BH 	;set date
BDOS_GET_TIME    EQU 2CH 	;get time
BDOS_SET_TIME    EQU 2DH 	;set time

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
	CALL	WRCH2   ;seems writing the vdu command (22) and then the mode, changes the graphic mode
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

                                                          