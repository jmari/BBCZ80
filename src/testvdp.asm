EXTERN    GOX	    ;GRAPHIC VIEWPORT ORIGIN  X in px
EXTERN    GOY	    ;GRAPHIC VIEWPORT ORIGIN  Y in px
EXTERN    GXW	    ;GRAPHIC VIEWPORT X WIDE in px
EXTERN    GYH	    ;GRAPHIC VIEWPORT Y HEIGHT in px

EXTERN    FPP       ;OPERATION


EXTERN    EXPRI     ;get basic parameters
EXTERN	  COMMA

PUBLIC  STORE5
PUBLIC  DLOAD5

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



ORG    100h       ; Inicio del código para un programa .COM
        

;
;CEXPRI:	CALL	COMMA
;	JP	EXPRI
;
       
        JP TEST
; Definición del string
mensaje:
        DB     "Hola Mundo", 13 


;MODE	- Move cursor to specified position.
;   	  Inputs: call basic EXPR (integer) Screen mode
;                 
; 	  Destroys: TO_DO
;

;MODE:   CALL	EXPRI		; get mode
;	LD	  A,L
LMODE: ;for testing 
        ;Reset ORIGIN
        LD     HL,0
        LD     (GOX),HL
        LD     (GOY),HL

        CP     2;
        JR     Z, SET_VIEWPORT_LR
        CP     6;
        JR     Z, SET_VIEWPORT_HR
        CP     7;
        JR     Z, SET_VIEWPORT_HR

SET_VIEWPORT_MR:
        LD     HL,256
        LD     (GXW), HL
        LD     HL,212
        LD     (GYH), HL
        JR CALL_CHMOD
SET_VIEWPORT_LR:
        LD     HL,256
        LD     (GXW), HL
        LD     HL,192
        LD     (GYH), HL
        JR CALL_CHMOD
SET_VIEWPORT_HR:
        ;Reset ORIGIN
        LD     HL,512
        LD     (GXW), HL
        LD     HL,212
        LD     (GYH), HL

CALL_CHMOD:
	LD     IY,(EXPTBL-1)       ;BIOS slot in iy
        LD     IX, CHGMOD    
        CALL   CALSLT
        RET



;
;ORIGIN x,y
;

;ORIGIN: 
;    CALL    EXPRI
;    EXX
;	PUSH	HL
;    CALL    CEXPRI
;	EXX
;    POP	DE
;        RET



;PCSR	- Move cursor to specified position.
;   	  Inputs: DE = horizontal position (LHS=0)
;                 HL = vertical position (TOP=0)
; 	  Destroys: A,D,E,H,L,F
;
        ;2 move cursor
PCSR:
        PUSH BC     ;STORES BC
        LD BC,HL
        LD  HL, GRPACYL
        LD (HL), B  ;accumulator Y coordenadas gráficas L
        dec HL
        LD (HL), C  ;accumulator Y coordenadas gráficas H
        dec HL
        LD BC,DE
        LD (HL), B  ;accumulator X coordenada grafica L
        dec HL
        LD (HL), C  ;accumulator X coordenadas gráficas H
        
        dec  HL     ;
        LD (HL), 0  ; 0fCB6   ;no se para que sirve
        dec HL
        LD (HL), 0  ; 0fCB5   ;no se para que sirve
        dec  HL     ;
        LD (HL), 0  ; 0fCB4   ;no se para que sirve
        dec HL
        LD (HL), 0  ; 0fCB3   ;no se para que sirve
       
  
        ; 3. Imprimir el string
        LD     DE, mensaje
PRINT_STRING:

        LD     A, (DE)  ; Carga la dirección del string
        CP     13

        JR     Z, EXIT_PRINT
        PUSH   DE
        LD     IY,(EXPTBL-1)       ;BIOS slot in iy
        LD     DE, SCRMOD   
        EX     AF,AF'
        LD     A, (DE)      ; A = display mode
        CP     0
        JR     Z,PS1
        EX     AF,AF'
        LD     IX, GRPPRT
        CALL   CALSLT        
        JR PS2
PS1:
        EX     AF,AF'
        LD     IX, CHPUT     ; print one char in text mode 
        CALL   CALSLT        ; call interslot subrutine
PS2:
        POP    DE
        INC    DE
        JR     PRINT_STRING
EXIT_PRINT:
        POP BC
        RET

WAIT4KEY:
        ; 4. Wait for a key 
        LD     IX, CHGET     ; wait for a character
        LD     IY,(EXPTBL-1) ; BIOS slot in iy
        CALL   CALSLT        ; call interslot rutine
        
        ; 5. Restore screen mode to SCREEN 0
        LD     A, 0       
        LD     HL, 0
        LD     IY,(EXPTBL-1)       ; BIOS slot in iy
        LD     IX, CHGMOD          ; restore text mode
        CALL   CALSLT              ; call interslot rutine
      
        RST	0               ; Retorna al sistema operativo
        

; SCALE_POS convert row/Column to pizel position
; Inputs: H = Fila (0-23), L = Columna (0-79)
; Outputs:  DE = Coordenadas de píxel (H = X, L = Y)
;          Los registros AF, DE se preservan
SCALE_TEXT_POS:
    PUSH AF
    PUSH BC

    ; get the screen mode
    LD A, (SCRMOD)      ; A = display mode

    ; calculates X (Columna * ancho_carácter)
    CP 2
    JR C, IS_TXT_MODE       ; Si A < 1 (Modos 0), es modo txt..ojo
    CP 6
    JR C, IS_40_COLUMNS     ; Si A < 6 (Modos 4 y 5), es de 40/32 columnas
    CP 8
    JR Z, IS_40_COLUMNS     ; Si A == 8, es de 32 columnas
    
    ; Lógica para 80 columnas (caracteres de 6 píxeles de ancho)
    LD A, L
    SLA A           ; A = Columna * 2
    ADD A, L        ; A = Columna * 3
    SLA A           ; A = Columna * 6
    LD D, A         ; D = Coordenada X
    JR Y_CALC

IS_40_COLUMNS:
    ; Lógica para 40 columnas (caracteres de 8 píxeles de ancho)
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

    POP BC
    POP AF
    RET

; SCALE_GRAPHIC_POS:cALCULATES PIXEL COORDS
;   	  Inputs: DE = horizontal position (LEFT=0..1279)
;                 HL = vertical position (bottom=0..1023)
; 	  Destroys: A,D,E,H,L,F
;
SCALE_GRAPHIC_POS:
    PUSH AF                         ;STACK AF
    PUSH BC                         ;STACK AF,BC
    PUSH HL                         ;STACK AF,BC,HL

    ;ARITHMETIC & LOGICAL OPERATORS:
    ;All take two arguments, in HLH"L'C & DED'E"B.
    ;Output in HLH'L'C
    ; Subrutina para escalar un valor rango 1280 al ancho del viewport.
    ;

    LD      HL, (GXW)     ; Carga el primer entero en el registro HL
    EXX                   ; Cambia a los registros alternos HL' tiene el ancho del viewport     
                          ; DE' contiene la porsicion x
    LD      HL, 0         ; Pone a cero HL (bits 31 al 16 del nº entero de 32 bits)
    LD      DE, 0         ; Pone a cero DE (bits 31 al 16 del nº entero de 32 bits)
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
    LD      DE, (GYH)     ; Carga el alto del view port
    EXX
    LD      HL,0
    LD      DE,0
    LD	    A,10
    CALL    FPP		;MULTIPLY

DIVIDE_BY_1024:
    EXX
    LD      DE, 1024
    EXX
    LD      DE, 0         ; Divisor está en DED'É'
    LD      BC,0          ;exponente tiene que ser 0 en ambos numeros
    LD      A,1
    CALL    FPP		  ;IBDIV         ; after the mul and div the number is 16 bits
    EXX
    ;after the mul and div the number is 16 bits HL contiene 
    POP     DE            ; carga X escalado 
    POP     BC
    POP     AF
    RET




IS_TXT_MODE:
    ;HL has yx
    LD A, H     ; Guarda el contenido de H en A
    LD H, L     ; Mueve el contenido de L a H
    LD L, A     ; Mueve el contenido de A (que era H) a L
    PUSH HL
    LD	IX,00C6H             ;address of BIOS routine
	LD     IY,(EXPTBL-1)       ;BIOS slot in iy
	CALL   001Ch      ; CALSLT
    POP DE
    POP BC
    POP AF
    RET


TEST:
    LD      A, 7   ;SET MODE 7
    CALL    LMODE
    LD      HL, 10     ;y = 10
    LD      DE, 25     ;x = 25  test bbc sends HL and DE but we only need one reg HL
    LD      H,L
    LD      L,E
    CALL    SCALE_TEXT_POS   ;DE has now scaled coordenates to pixel screen
    LD      HL, DE      ;L has y
    LD      E, H        ;E has x
    LD      H,0
    LD      D,0
    CALL    PCSR
    ;test scale
    LD      HL,1000         ;Y 207  207   187
    LD      DE,1200         ;X 243  487   243
    CALL    SCALE_GRAPHIC_POS
 
    LD      HL,00           ;Y 0 
    LD      DE,00           ;X 0
    CALL    SCALE_GRAPHIC_POS

    LD      HL,512         ;Y 106   106   96 
    LD      DE,640         ;X 130   260  130
    CALL    SCALE_GRAPHIC_POS
    LD      HL,1023        ;Y 211   211  191
    LD      DE,1279        ;X 255   511  256
    CALL    SCALE_GRAPHIC_POS
    CALL    WAIT4KEY


STORE5:
        ret
DLOAD5:
        ret