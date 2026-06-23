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
VDU_DEVICE_ID EQU 150

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
    CALSLT      EQU     001Ch       ;Call inter-slot rom subrutine in IX
    GRPPRT      EQU     008Dh       ;Function : Displays a character on the graphic screen
                                    ;Input    : A  - ASCII value of the character to print 
 	POSIT		EQU  	00C6h		;Function: 	Moves the cursor
									;Input: 	H = X-coordinate of the cursor, L for the Y-coordinate
									;Output: 	None
; --- MSX DOS SUBROUTINES
	BDOS_GET_DATE    EQU 2AH 	;get date
	BDOS_SET_DATE    EQU 2BH 	;set date
	BDOS_GET_TIME    EQU 2CH 	;get time
	BDOS_SET_TIME    EQU 2DH 	;set time
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

; MSX HOOKS
	EXTBIO           EQU 0FFCAh    ; Vector de salto a la Extended BIOS
;***************************************************************************************

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
	PUBLIC  WRITE_VDU
	PUBLIC  PCSR
;



ESC	EQU	1BH
TBY	EQU	0FH
TTO	EQU	0B8H
TFILL	EQU	03H
;
SCRAP:	DEFS	31
	DEFB	0

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
VDU_CMD:
	LD A,E          
	CP 0FFH         ; if the byte is FF it is requesting for a key input
	JR Z, BDOS_CALL
VDU_CMD_W:
	LD   D, VDU_DEVICE_ID
	LD   L, E
	LD   E,1 		; CMD 1 WRITE TO VDU
	CALL EXTBIO  
	POP	IY
	POP	IX
	RET





;-----------------MINIMUN ROUTINES FOR OUTPUT CHARS WITHOUT GRX.BIN-------

WRITE_VDU:
;------We need this WRITE_VDU for VDU Command because 
;------cmos.OSWRCH (cmos.asm) uses 06h cpm call 
;------06h subrutine cannot send FF to the console output
;------and we need that to send logical coords to the VDU
;------vdudrv.OSWRCH uses 02H subrutine so only writes to console
;------name changed to WRITE_VDU	 
	PUSH AF
	PUSH DE
	LD  E, A
	LD 	A ,02H
	CALL BDOS0	
	POP DE
	POP AF
	RET


;-----------------------------RT CLOCK ---------------------------------------------------

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

;--------------------------------------VDU OPERATIONS--------------------------------------
;
;PCSR	- Move cursor to specified position.
;   	  Inputs: DE = horizontal position (LHS=0)
;                 HL = vertical position (TOP=0)
; 	  Destroys: A,D,E,H,L,F
;
PCSR:
		LD	A,31
		CALL	WRITE_VDU
		LD	A,E   
		CALL	WRITE_VDU
		LD	A,L
		JP		WRITE_VDU  ;writes the vdu command  and choords
		JP  EXPR3 ;I DON KNOW WHY CALLING EXPR3 ANYWAY WORKS......


;POINT - var=POINT(x,y)
; read the color of the pixel xy
POINT:	CALL	EXPRI
	EXX
	PUSH	HL
	CALL	CEXPRI
	EXX
	POP	DE
	CALL	BRAKET
	;replaced system call by VDU 128 cmd
	LD	C,129
	CALL	VDU25_CMD
	;colour should be in A
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
	CALL	WRITE_VDU
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
	CALL	WRITE_VDU
	LD	A,1
	CALL	WRITE_VDU
	LD	A,C
	LD	B,8
CSRGO1:	CALL	WRITE_VDU
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
	CALL	VDU25_CMD
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
	CALL	VDU25_CMD
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
	CALL	VDU25_CMD
	POP	DE		;a
	PUSH	DE
	LD	HL,0
	LD	C,L		; PLOT 0 - Move relative
	CALL	VDU25_CMD
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
PLOT4:	CALL	VDU25_CMD
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
	CALL	VDU25_CMD
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
	CALL	VDU25_CMD
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
	CALL	VDU25_CMD		; side 1
	POP	HL
	PUSH	DE
	LD	DE,0
	CALL	VDU25_CMD		; side 2
	POP	DE
	PUSH	HL
	XOR	A
	LD	L,A
	LD	H,A
	SBC	HL,DE
	EX	DE,HL
	LD	L,A
	LD	H,A
	CALL 	VDU25_CMD		; side 3
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


VDU25_CMD:	LD	B,25
	WRCH6:	LD	A,B
		CALL	WRITE_VDU
	WRCH5:	LD	A,C
		CALL	WRITE_VDU
	WRCH4:	LD	A,E
		CALL	WRITE_VDU
	WRCH3:	LD	A,D
		CALL	WRITE_VDU
	WRCH2:	LD	A,L   
		CALL	WRITE_VDU
		LD	A,H
		JP	WRITE_VDU
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

                                                          