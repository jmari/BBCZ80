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
	;GLOBAL	CLRSCN      implemented in dist.asm
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
;
SCRAP:	DEFS	31
	DEFB	0
TEMP_DEST: DEFS 2
    DEFB    0
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
BDOS_GET_DATE    EQU 2AH 	;get date
BDOS_SET_DATE    EQU 2BH 	;set date
BDOS_GET_TIME    EQU 2CH 	;get time
BDOS_SET_TIME    EQU 2DH 	;set time
;
;GETIMS	- Read real-time clock as string.
;  	  Outputs:  TIME$ in string accumulator
;                   E = string length (25)
; 	  Destroys: A,B,C,D,E,H,L,F
;
; USES Function STR - convert numeric value to ASCII string.
;   Inputs: HLH'L'C = integer or floating-point number
;           DE = address at which to store string
;           IX = address of @% format control
;    LD	    A,37
;    CALL    FPP		  ;STR          ; HLH'L' contiene la multiplicacion
DAY_OF_WEEK:	DEFM	"Sun.Mon.Tue.Wed.Thu.Fri.Sat." ; four chars per day
DAYFORMAT:	DEFM	"@00"
MONTHS:	DEFM	"Jan Feb Mar Apr May Jun Jul Ago Sep Oct Nov Dec" ; four chars per month
YEARFORMAT:	DEFM	"@00"

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
;
;
;CLRSCN	- Clear screen.
; 	  Destroys: A,D,E,H,L,F
;
CLRSCN:	LD	A,0CH
	JP	OSWRCH


;POINT - var=POINT(x,y)
;
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
;
ADVAL:	CALL	ITEMI
	EXX
	LD	A,128
	CALL	OSBYTE
	XOR	A
	JR	RETEXX
;
;MODEFN - var=MODE
;
MODEFN:	LD	A,135
	CALL	OSBYTE
	LD	L,H
RETU8:	XOR	A
	LD	H,A
	JR	RETEXX
;
;WIDFN - var=WIDTH
;
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
	LD	L,22
	CALL	WRCH2
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

                                                          