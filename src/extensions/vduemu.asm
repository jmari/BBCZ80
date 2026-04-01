;	TITLE	COPYRIGHT (C) J.MARI-AGUIRRE 2025-2042
;
; VDU DRIVER FOR MSX-DOS
; VERSION 1.0, 12-07-2025
;
;
	PUBLIC  VDU_CMD_W

; --- EXTERN ---
    EXTERN  FPP       ;for Math subrutines operation
; VDP GENERIC COMMAND VARS  
	;GENERIC CMD AREA
	EXTERN 	StartX 	    
	EXTERN 	StartY
	EXTERN 	EndX
	EXTERN 	EndY
	EXTERN 	Maj
	EXTERN 	Min
	EXTERN 	Color
	EXTERN 	lineFlags
	EXTERN 	LogOp
	EXTERN 	vdp_cmd


	EXTERN  VDU_GVXW	
	EXTERN  VDU_GVXH	
	EXTERN 	VDU_GVOX
	EXTERN 	VDU_GVEX
	EXTERN	VDU_GVOY
	EXTERN	VDU_GVEY
	EXTERN 	CSRXTP
	EXTERN	CSRYTP
	EXTERN 	PALETTE

; VDP SUBROUTINES
	EXTERN 	VDP_DRAW_GENERIC_CMD
	EXTERN	SCALE_TEXT_POS
	EXTERN	SCALE_GRAPHIC_POS
	EXTERN  PCSR
	EXTERN 	VSCROLL
	EXTERN	CLS
	EXTERN	CPL_HL
	EXTERN	findPhysicalColor
	EXTERN	ipalette
	EXTERN	read_vdp_status_register
	EXTERN 	wait_vdp_ready
	EXTERN	plotSinglePoint
	EXTERN	chColors


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

; --- MSX ROM BIOS SUBRUTINES
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

	;
	CPM	EQU	5
	; --- MSX DOS SUBROUTINES
	BDOS_CONSOLE_OUTPUT EQU 02H

	; --- KEYS ---
	BEL		EQU	07H
	CUP		EQU	1EH	;CURSOR UP
	CDOWN	EQU	1FH	;CURSOR DOWN
	STLN	EQU	0BH	;START OF LINE
	EDLN	EQU	0Eh	;END OF LINE
	DENL	EQU	05H	;DELETE TO END OF LINE
	BACK	EQU	08H		;BACKSPACE & DELETE
	CR 		EQU 0DH
	LF 		EQU 0AH
	DBACK	EQU	00H	;DEL TO START OF LINE
	CLF		EQU	1DH	;CURSOR LEFT
	CRG		EQU	1CH ;CURSOR RIGHT
	DEL		EQU	7FH	;DELETE CHARACTER
	INS		EQU	12h	;INS/OVR TOGGLE
	ESC		EQU	1BH
	TBY		EQU	0FH
	TTO		EQU	0B8H
	TFILL	EQU	03H



    

;-----------------------------VDU DRIVER---------------------------------------
;*******************TRIANGLE SECTION************************
; ==========================================================
; ZONA DE DATOS (RAM) PARA TRIANGULO
; ==========================================================

; --- Puntos de entrada (Rellenar antes de llamar) ---
	P0_X: DEFW 0  
	P0_Y: DEFW 0
	P1_X: DEFW 0
	P1_Y: DEFW 0
	P2_X: DEFW 0
	P2_Y: DEFW 0

; --- Interfaz con  rutina de línea ---
	USER_X0: DEFW 0 ; Start X
	USER_Y0: DEFW 0 ; Start Y (e Y actual)
	USER_X1: DEFW 0 ; End X
	USER_Y1: DEFW 0 ; End Y (Ignorado si es horizontal, pero lo rellenamos por si acaso)

	; --- Estructura de Rastreo de Bordes (Bresenham) ---
	; Necesitamos dos instancias: una para el lado largo (A) y otra para el corto (B)
	; Offset:
	; +0 Current_X (DW)
	; +2 Step_X    (DW) -> Pasos enteros por línea (ej: 2 pixeles) con signo
	; +4 Error_Acc (DW) -> Acumulador de error actual
	; +6 Error_Adj (DW) -> Cuánto restar al error (Delta Y)
	; +8 Error_Inc (DW) -> Cuánto sumar al error (Resto de DX/DY)
	; +10 Sign_X   (DW) -> +1 o -1 (para el ajuste del error)

	EDGE_LONG:  DEFS 12 ; Estructura para el lado P0 -> P2
	EDGE_ACTIVE: DEFS 12 ; Estructura para el lado P0->P1 y luego P1->P2
	NOP
	NOP
CLIP_HORIZONTAL:
    ; ---------------------------
    ; 1. PROCESAR START X
    ; ---------------------------
    LD HL, (StartX)     ; HL = Valor a comprobar
    CALL CLIP_VAL_HL    ; Llamamos a la rutina de recorte
    LD (StartX), HL     ; Guardamos el resultado corregido

    ; ---------------------------
    ; 2. PROCESAR END X
    ; ---------------------------
    LD HL, (EndX)       ; HL = Valor a comprobar
    CALL CLIP_VAL_HL    ; Llamamos a la rutina de recorte
    LD (EndX), HL       ; Guardamos el resultado corregido
    RET

; ==========================================================
; CLIP_VAL_HL (VERSIÓN SIGNED)
; Entrada: HL = Valor X a comprobar (Puede ser negativo)
; Salida:  HL = Valor recortado (entre GVOX y GVEX)
; ==========================================================
CLIP_VAL_HL:
    ; --- CHECK MIN (VDU_GVOX) ---
    LD DE, (VDU_GVOX)   ; Cargar Mínimo (ej. 0)
    CALL COMPARE_HL_DE ; <--- CAMBIO CLAVE AQUÍ
    ; Si Carry está ON, significa que HL < DE (Signed)
    ; Es decir, HL es negativo o menor que el borde izquierdo.
    JR NC, @CHECK_MAX   ; Si HL >= Min, pasamos a ver el Máx.
    
    ; CORRECCIÓN MÍNIMO
    EX DE, HL           ; HL toma el valor de DE (Mínimo)
    RET                 ; Retornamos (Ya recortamos al mínimo)

@CHECK_MAX:
    ; --- CHECK MAX (VDU_GVEX) ---
    LD DE, (VDU_GVEX)   ; Cargar Máximo (ej. 255 o 320)
    CALL COMPARE_HL_DE ; <--- CAMBIO CLAVE AQUÍ
    ; Si Carry está ON, significa HL < Max. Es válido.
    RET C               
    ; Si Zero está ON, significa HL == Max. Es válido.
    RET Z               

    ; CORRECCIÓN MÁXIMO
    EX DE, HL           ; HL toma el valor de DE (Máximo)
    RET

; ==========================================================
; COMPARE_HL_DE
; Compara HL con DE usando aritmética de COMPLEMENTO A 2.
; Funciona correctamente con valores negativos.
; C set si HL < DE (ej. -5 < 0)
; NC set si HL >= DE
; Z set si HL == DE
; Preserva HL y DE.
; ==========================================================
COMPARE_HL_DE:
    PUSH HL
    PUSH DE
    
    ; --- TRUCO DEL BIT DE SIGNO ---
    ; Invertimos el bit 7 de H y D.
    ; Esto mueve el rango de -32768..+32767 a 0..65535
    ; haciendo que una resta normal funcione para comparar magnitud.
    LD A, H
    XOR $80
    LD H, A
    
    LD A, D
    XOR $80
    LD D, A
    
    ; --- COMPARACIÓN ---
    OR A            ; Limpiar Carry
    SBC HL, DE      ; Resta normal (ahora con los rangos corregidos)
    
    POP DE          ; Restauramos los valores originales
    POP HL
    RET
; ==========================================================
; RUTINA PRINCIPAL: FILL_TRIANGLE
; ==========================================================
FILL_TRIANGLE:
    ; 1. Ordenar vértices
    call SORT_POINTS

    ; 2. Calcular Altura Total y preparar EDGE_LONG
    ld hl, (P2_Y)
    ld de, (P0_Y)
    or a
    sbc hl, de
    ret z           ; Altura 0
    
    ld b, h
    ld c, l         ; BC = Altura Real (P2.y - P0.y) <--- IMPORTANTE
    ld iy, EDGE_LONG
    ld hl, (P2_X)
    ld de, (P0_X)
    call SETUP_EDGE_BRESENHAM

    ; 3. --- MITAD SUPERIOR ---
    ld hl, (P1_Y)
    ld de, (P0_Y)
    or a
    sbc hl, de
    jr z, @setup_bottom ; Triángulo con parte superior plana

    push hl         ; Guardar altura superior (DY)
    ld b, h
    ld c, l         ; BC = Altura superior real
    ld iy, EDGE_ACTIVE
    ld hl, (P1_X)
    ld de, (P0_X)
    call SETUP_EDGE_BRESENHAM
    
    pop bc          ; BC = Altura para el bucle
    ld hl, (P0_Y)
    ld (USER_Y0), hl
    call RENDER_LOOP

    ; 4. --- MITAD INFERIOR ---
@setup_bottom:
    ld hl, (P2_Y)
    ld de, (P1_Y)
    or a
    sbc hl, de
    ret z           ; Parte inferior plana

    push hl         ; Guardar altura inferior (DY)
    ld b, h
    ld c, l         ; BC = Altura inferior real
    ld iy, EDGE_ACTIVE
    ld hl, (P2_X)
    ld de, (P1_X)
    call SETUP_EDGE_BRESENHAM

    pop bc          ; BC = Altura para el bucle
    ld hl, (P1_Y)
    ld (USER_Y0), hl
    call RENDER_LOOP
    ret

; Entrada: BC = Número de líneas (Altura), USER_Y0 = Y actual
RENDER_LOOP:
    ld a, b
    or c
    ret z
@loop:
    push bc

    ; 1. Cargar coordenadas X actuales de los bordes
    ld hl, (EDGE_LONG + 0)   ; Current_X Long
    ld (USER_X0), hl
    
    ld hl, (EDGE_ACTIVE + 0) ; Current_X Active
    ld (USER_X1), hl
    
    ld hl, (USER_Y0)
    ; -----    HL HAS CURRENT Y
    ; --- Comparar con el límite superior (Y < V_YMIN) ---
    LD DE, (VDU_GVOY)
    CALL COMPARE_HL_DE  ; HL=Y, DE=V_YMIN. Carry si Y < V_YMIN
    JR C, @NOTHING2DRAWORPUSH    ; Si es menor, está por encima: No dibujar
    
    ; --- Comparar con el límite inferior (Y > V_YMAX) ---
    LD DE, (VDU_GVEY)
	INC DE
    CALL COMPARE_HL_DE  ; Carry si Y > V_MAX
    JR NC, @NOTHING2DRAWORPUSH    ; Si es mayor, está por debajo: No dibujar
	inc hl
    ld (USER_Y1), hl   
   
    ; 2. --- RUTINA VDP---
    ; Tu rutina lee USER_X0, Y0, USER_X1, Y1 y dibuja
    push ix
    push iy
	
	; ESTA ES mi rutina
	LD IY, (USER_X0)
	LD (StartX), IY
	LD IY, (USER_Y0)
	LD (StartY), IY
	LD IY, (USER_X1)
	LD (EndX), IY
	LD IY, (USER_Y1)
	LD (EndY), IY
	CALL CLIP_HORIZONTAL
	LD HL,(StartX)
	LD DE,(EndX)
	CALL COMPARE_HL_DE
	JR Z,@NOTHING2DRAW
	LD IY,vdp_cmd
	LD (IY),10000000b    ;HMMV cmd
	CALL VDP_DRAW_GENERIC_CMD
	;----fin de mi rutina
@NOTHING2DRAW:
    pop iy
    pop ix
@NOTHING2DRAWORPUSH:
	
    ; 3. Avanzar Bresenham para el siguiente Y
    ld iy, EDGE_LONG
    call UPDATE_EDGE
    
    ld iy, EDGE_ACTIVE
    call UPDATE_EDGE
    
    ; 4. Incrementar Y y bucle
    ld hl, (USER_Y0)
    inc hl
    ld (USER_Y0), hl
    
    pop bc
    dec bc
    ld a, b
    or c
    jp nz, @loop
    ret

; ----------------------------------------------------------
; SETUP_EDGE_BRESENHAM
; Entrada: 
;   IY = Puntero a estructura EDGE
;   HL = X Destino
;   DE = X Origen
;   BC = Y Destino (se usa para calcular altura con Y Origen implícito)
;   (Nota: Y Origen no se pasa explícitamente porque calculamos DY con P0_Y o P1_Y fuera,
;    pero para simplificar, pasamos DY directamente en BC en esta versión optimizada)
;
;   CORRECCION: Para hacerlo genérico, recalcularemos DY dentro.
;   Requerimos: DE = X Start, HL = X End. 
;   Necesitamos saber DY. Asumiremos que el caller ya calculó DY y lo pasa en BC.
;   Entrada Real: DE=X_Start, HL=X_End, BC=DY (Altura > 0), IY=Struct
; ----------------------------------------------------------
; ----------------------------------------------------------
; SETUP_EDGE_BRESENHAM
; Entrada: DE=X_Start, HL=X_End, BC=DY, IY=Struct
; ----------------------------------------------------------
SETUP_EDGE_BRESENHAM:
    ; 1. Guardar X inicial
    ld (iy+0), e
    ld (iy+1), d   ; Current_X = X_Start

    ; 2. Guardar DY (BC)
    ld (iy+6), c
    ld (iy+7), b   ; DY

    ; 3. Calcular DX = X_End - X_Start
    or a
    sbc hl, de     ; HL = DX
    
    ; 4. Determinar signo de DX y obtener valor absoluto
    bit 7, h
    jr nz, @negative

@positive:
    ld (iy+10), 1  ; Sign_X = 1
    ld (iy+11), 0
    jr @calc_err

@negative:
    ld (iy+10), -1 ; Sign_X = -1
    ld (iy+11), -1
    ; HL = ABS(DX) -> Negar HL
    ex de, hl
    ld hl, 0
    or a
    sbc hl, de

@calc_err:
    ; Ahora HL = |DX| y BC = DY
    ld (iy+2), l   ; Guardamos |DX| en el lugar de Error_Inc
    ld (iy+3), h
    
    ; Inicializar Error_Acc. 
    ; Para un centrado perfecto: Error = |DX| / 2 o simplemente 0
    ld (iy+4), 0   
    ld (iy+5), 0
    ret

; ----------------------------------------------------------
; UPDATE_EDGE
; Avanza la X para la siguiente línea Y
; ----------------------------------------------------------
UPDATE_EDGE:
    ; Error_Acc += |DX|
    ld l, (iy+4)
    ld h, (iy+5)
    ld c, (iy+2)   ; |DX|
    ld b, (iy+3)
    add hl, bc
    
    ; BC = DY
    ld c, (iy+6)
    ld b, (iy+7)

@test_step:
    ; ¿Error_Acc >= DY?
    or a
    sbc hl, bc
    jr c, @finish   ; Si es menor, no hay más pasos de X en esta Y

    ; X += Sign_X
    push hl
    ld l, (iy+0)
    ld h, (iy+1)
    ld e, (iy+10)  ; Sign_X
    ld d, (iy+11)
    add hl, de
    ld (iy+0), l
    ld (iy+1), h
    pop hl
    
    ; Repetir para pendientes > 45º (donde |DX| > DY)
    jr @test_step

@finish:
    ; Restaurar Error_Acc (el SBC restó DY de más)
    add hl, bc
    ld (iy+4), l
    ld (iy+5), h
    ret

; ----------------------------------------------------------
; SORT_POINTS (CORREGIDO PARA SIGNED 16-BIT)
; Ordena P0, P1 y P2 para que P0 sea el superior (menor Y) 
; y P2 el inferior (mayor Y).
; ----------------------------------------------------------
SORT_POINTS:
    ; --- Comparar P0 y P1 ---
    ld hl, (P0_Y)
    ld de, (P1_Y)
    CALL CMP_SIGNED     ; Carry Set si HL < DE (Signed)
    jr c, @skip1        ; Si P0 < P1, está bien, saltar
    jr z, @skip1        ; Si son iguales, saltar
    call SWAP_P0_P1     ; Si P0 > P1, intercambiar

@skip1:
    ; --- Comparar P1 y P2 ---
    ld hl, (P1_Y)
    ld de, (P2_Y)
    CALL CMP_SIGNED     ; Carry Set si HL < DE (Signed)
    jr c, @skip2
    jr z, @skip2
    call SWAP_P1_P2

@skip2:
    ; --- Re-comparar P0 y P1 ---
    ld hl, (P0_Y)
    ld de, (P1_Y)
    CALL CMP_SIGNED
    ret c
    ret z
    call SWAP_P0_P1
    ret

; ==========================================================
; CMP_SIGNED
; Compara HL vs DE considerando signo (Complemento a 2)
; Entrada: HL, DE
; Salida: 
;   Carry = 1 si HL < DE
;   Carry = 0 si HL >= DE
;   Zero  = 1 si HL == DE
; Nota: Preserva HL y DE intactos
; ==========================================================
CMP_SIGNED:
    PUSH HL
    PUSH DE
    
    ; Invertimos el bit de signo (Bit 7 de H y D)
    ; Esto desplaza el rango de -32768..32767 a 0..65535
    LD A, H
    XOR $80
    LD H, A
    
    LD A, D
    XOR $80
    LD D, A
    
    ; Ahora comparamos normalmente
    OR A            ; Limpiar Carry
    SBC HL, DE      ; Comparación estándar
    
    POP DE          ; Recuperamos valores originales
    POP HL          ; (POP no afecta a los flags C ni Z)
    RET

; --- Bloques de intercambio ---



SWAP_P0_P1:
    ld hl, P0_X
    ld de, P1_X
    jr __do_swap

SWAP_P1_P2:
    ld hl, P1_X
    ld de, P2_X

__do_swap:
    ; Intercambia 4 bytes (X low, X high, Y low, Y high)
    ld b, 4
@loop:
    ld a, (hl)
    ld c, (de)
    ld (hl), c
    ld (de), a
    inc hl
    inc de
    djnz @loop
    ret

;***************************************************************************************

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
	DEFW VDU25 
	DEFW VDU26
	DEFW VDU27
	DEFW VDU28
	DEFW VDU29
	DEFW VDU30
	DEFW VDU31
	;DEFW VDU127



;Command VDU
; Input: E is the input byte
VDU_CMD_W:
	LD A,(VDU_ARGC)	; how many arguments are we waiting?
	CP 0			; if not 0 read this byte is an argument 
	JR NZ,VDU_READ_PARAMS_MODE
	LD A,E  
	CP 32           ; else if the byte is ge to 31 it is not a VDU cmd
	JP NC, GOSWRCH
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
	CALL CLS
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
	call findPhysicalColor
	LD D, A         ;D holds the index (physical color)
	LD A,(VDU_ARGV)         ;Logical foreground color  byte
	AND 11110000B
	RRA
	RRA
	RRA
	RRA
	call findPhysicalColor
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
	call chColors
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
	CALL findPhysicalColor
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
	CALL ipalette			;sets the RGB palete register R#E
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
		DEC HL
		LD     (VDU_GVEX), HL
		LD     HL,212
		LD     (VDU_GVXH), HL
		DEC HL
		LD     (VDU_GVEY), HL

		JR CALL_CHMOD
	
	SET_VIEWPORT_LR:
		LD     HL,256
		LD     (VDU_GVXW), HL
		DEC HL
		LD     (VDU_GVEX), HL
		LD     HL,192
		LD     (VDU_GVXH), HL
		DEC HL
		LD     (VDU_GVEY), HL
		JR CALL_CHMOD

	SET_VIEWPORT_HR:
		LD     HL,512
		LD     (VDU_GVXW), HL
		DEC HL
		LD     (VDU_GVEX), HL
		LD     HL,212
		LD     (VDU_GVXH), HL
		DEC HL
		LD     (VDU_GVEY), HL
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
VDU25:

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
		ld a, h    ; Load high byte into accumulator
    	cpl        ; Invert all bits (1's complement)
    	ld h, a    ; Store back to H
    	ld a, l    ; Load low byte into accumulator
    	cpl        ; Invert all bits
    	ld l, a    ; Store back to L
		INC HL
		LD BC,(GRPACY);	Y Graphics Accumulator
		ADD HL,BC
		LD IX, (GRPACX);
		ADD IX,DE
		PUSH IX
		POP DE
		JR DO_NOT_NEED_INVERT
PLOT_ABSOLUTE:
	;INVERT AXIS Y
    LD 		BC,(VDU_GVEY)					        
	SBC 	HL,BC
	CALL    CPL_HL
	
DO_NOT_NEED_INVERT:	  ; HL contiene Y escalado e invertido, H siempre es 0
	LD A,(VDU_ARGV+4) 		;loads plot mode AGAIN
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
		CALL VDP_DRAW_GENERIC_CMD
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
	BIT 3,A     ;(BIT 6 AND 3 )
	JP NZ,PLOT_FILL_HORIZONTAL_LINE
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
		CALL plotSinglePoint
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

		LD BC, (GXPOSH)    ;desde 1er punto
		LD (P0_X), BC
		LD BC, (GYPOSH)
		LD (P0_Y), BC	
		LD BC,(GRPACX)      ;hasta segundo
		LD (P1_X),BC
		LD BC,(GRPACY)
		LD (P1_Y),BC
		LD (P2_X),DE
		LD (P2_Y),HL
		call FILL_TRIANGLE
		RET
	PLOT_TRIANGLE_IFG:
	PLOT_TRIANGLE_BGC:
		LD BC, (GXPOSH)    ;desde 1er punto
		LD (StartX), BC
		LD BC, (GYPOSH)
		LD (StartY), BC
		LD (EndX),DE		;hasta tercero
		LD (EndY),HL
		CALL VDP_DRAW_GENERIC_CMD
		LD BC, (EndX)		;desde tercero
		LD (StartX), BC
		LD BC, (EndY)
		LD (StartY), BC
		LD BC,(GRPACX)      ;hasta segundo
		LD (EndX),BC
		LD BC,(GRPACY)
		LD (EndY),BC
		CALL VDP_DRAW_GENERIC_CMD
		LD BC, (EndX)		;desde segundo
		LD (StartX), BC	
		LD BC, (EndY)
		LD (StartY), BC
		LD BC,(GXPOSH)      ;hasta primero
		LD (EndX),BC
		LD BC,(GYPOSH)
		LD (EndY),BC
		CALL VDP_DRAW_GENERIC_CMD
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
		LD (IY),10000000b    ;HMMV cmd
		LD BC, (GRPACX)
		LD (StartX), BC
		LD BC, (GRPACY)
		LD (StartY), BC
		LD (EndX),DE
		LD (EndY),HL
		CALL VDP_DRAW_GENERIC_CMD
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
		CALL VDP_DRAW_GENERIC_CMD
		POP DE
		POP HL
		LD (StartY),HL
		LD BC,(GRPACX) 
		LD (StartX),BC
		LD (EndX),DE		
		LD (EndY),HL
		PUSH HL
		PUSH DE
		CALL VDP_DRAW_GENERIC_CMD
		POP DE
		POP HL
		LD (StartY),HL
		LD (StartX),DE
		LD BC,(GRPACY)
		LD (EndY),BC
		LD (EndX),DE
		PUSH HL
		PUSH DE
		CALL VDP_DRAW_GENERIC_CMD
		POP DE
		POP HL
		LD BC,(GRPACY)
		LD (StartY),BC
		LD (StartX),DE
		LD (EndY),BC
		LD BC,(GRPACX)      ;HASTA PUNTO ANTERIOR
		LD (EndX),BC
		CALL VDP_DRAW_GENERIC_CMD
		RET




PLOT_FILL_HORIZONTAL_LINE:
	LD IY,vdp_cmd
	LD (IY),01100000b    ;line cmd
	AND 00000011b
	CP 0 					;move relative command
	JP Z, MOVETO
	CP 1 					;draw horizontal line at pos in foreground color
	JR Z, PLOT_FILL_HORIZONTAL_LINE_FGC
	CP 2 					;draw horizontal line  at pos in inverse foreground color
	JR Z, PLOT_FILL_HORIZONTAL_LINE_IFG
	CP 3 					;draw horizontal line  at pos in background color
	JR Z, PLOT_FILL_HORIZONTAL_LINE_BGC
	;CMD IMPLEMENTATION
	PLOT_FILL_HORIZONTAL_LINE_FGC:
	PLOT_FILL_HORIZONTAL_LINE_IFG:
	PLOT_FILL_HORIZONTAL_LINE_BGC:
		LD (StartX), DE
		LD (StartY), HL
		LD BC,00
		LD (EndX),BC
		INC HL
		LD (EndY),HL
		LD A,(Color)  ;stores current color in STACK
		PUSH AF
		LD A,(BAKCLR) ;sets color to bg color
		LD (Color),A
		CALL VDP_DRAW_GENERIC_CMD	;SEARCHES TO THE LEFT
		CALL wait_vdp_ready
		LD A, 8
		CALL read_vdp_status_register ;reads result of register 8
		LD L,A
		LD A, 9
		CALL read_vdp_status_register ;reads result of register 9
		AND 00000001B
		LD H,A
		PUSH HL
		LD BC,(VDU_GVXW)
		DEC BC
		LD (EndX),BC
		CALL VDP_DRAW_GENERIC_CMD   ;SEARCHES TO THE RIGHT
		CALL wait_vdp_ready
		LD A, 8
		CALL read_vdp_status_register;reads result of register 8
		LD E,A
		LD A, 9
		CALL read_vdp_status_register;reads result of register 9
		AND 00000001B
		LD D,A
		POP HL
		POP AF
		LD (Color),A  
		LD A, H
    	XOR D          ; Compara parte alta
    	JR NZ, HL_NO_IGUAL_DE
    	LD A, L
    	XOR E          ; Compara parte baja
    	JR NZ, HL_NO_IGUAL_DE
		ret
HL_NO_IGUAL_DE:
    ; HL es exactamente igual a DE
		ld (StartX),HL
		ld (EndX),DE
		LD IY,vdp_cmd
		LD (IY),10000000b    ;HMMV VDP->VRAM cmd
		CALL VDP_DRAW_GENERIC_CMD
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
    LD 		BC,(VDU_GVEY)					        
	SBC 	HL,BC		  ; HL contiene Y escalado e invertido, H siempre es 0
	CALL CPL_HL
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






                                                          