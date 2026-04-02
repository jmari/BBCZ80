;	TITLE MSX VDP DRAWING ROUTINES	
;   COPYRIGHT (C) J.MARI
;   V1.0, 27-01-2026
;PATCH FOR BBC BASIC TO MSX-DOS
;* ACORN COMPUTERS Z80 TUBE VERSION  *
;(C) COPYRIGHT R.T.RUSSELL, 02-01-1984
;VERSION 5.0, 12-07-2024


; --- PUBLIC ---
;	PUBLIC GENERIC CMD AREA
	PUBLIC 	StartX 	    
	PUBLIC 	StartY
	PUBLIC 	EndX
	PUBLIC 	EndY
	PUBLIC 	Maj
	PUBLIC 	Min
	PUBLIC 	Color
	PUBLIC 	lineFlags
	PUBLIC 	LogOp
	PUBLIC 	vdp_cmd

; PUBLIC VIEWPORT VARS
	PUBLIC  VDU_GVXW	
	PUBLIC  VDU_GVXH
	PUBLIC 	VDU_GVOX
	PUBLIC 	VDU_GVEX
	PUBLIC	VDU_GVOY
	PUBLIC	VDU_GVEY

; PUBLIC VDP SUBROUTINES
	PUBLIC 	VDP_DRAW_GENERIC_CMD
	PUBLIC	SCALE_TEXT_POS
	PUBLIC	SCALE_GRAPHIC_POS
	PUBLIC	PALETTE
	PUBLIC  PCSR
	PUBLIC 	VSCROLL
	PUBLIC	CLS
	PUBLIC	findPhysicalColor
	PUBLIC	ipalette
	PUBLIC	read_vdp_status_register
	PUBLIC 	wait_vdp_ready
	PUBLIC	plotSinglePoint
	PUBLIC	chColors
; UTILITIES
	PUBLIC	CPL_HL
; --- EXTERN ---

    EXTERN  FPP       ;for Math subrutines operation

; VDP I/O PORTS
; --- Definiciones de Puertos y Registros VDP ---
	VDP_DATA_PORT   EQU 98h
	VDP_CTRL_PORT   EQU 99h
	VDP_INDR_PORT   EQU 9Bh

	R36 EQU 36
	R38 EQU 38
	R40 EQU 40
	R42 EQU 42
	R44 EQU 44
	R46 EQU 46   ; CMD register when using this command set

;
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
;----------Graphic Viewport variables----------
	VDU_GVOX:	DEFW 0000H	
	VDU_GVOY:	DEFW 0000H
	VDU_GVXW: 	DEFW 0000H
	VDU_GVXH:	DEFW 0000H
	VDU_GVEX: 	DEFW 0000H
	VDU_GVEY:	DEFW 0000H
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



; --- Espacio de datos para comandos VDP simplificados ---
	StartX:     DW 50   ;0
	StartY:     DW 50   ;2
	EndX:       DW 200  ;4
	EndY:       DW 150  ;6
	Maj:		DW 150  ;8
	Min:		DW 100  ;10
	Color:		DB 0    ;12
	lineFlags:	DB 0    ;13
	LogOp:		DB 0    ;14
	vdp_cmd:	DB 0	;15

; --- SCROLL POSITION ---

	SCROLL_Y_POS: DEFB 00H



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


VDP_DRAW_GENERIC_CMD:
	push ix
	ld ix,StartX
	ld (ix+13),0 ;reset flags
	ld hl,(ix)
	ld de,(ix+4)
	or a
	set 2,(ix+13)
	SBC HL, DE
	bit 7,H
	jr Z,@positive_left_flag
	res 2,(ix+13)
	call CPL_HL    ;nbX es ahora positivo 
@positive_left_flag:
	ld (ix+8),hl
	ld hl,(ix+2)
	ld de,(ix+6)
	or a
	set 3,(ix+13)
	SBC HL, DE
	bit 7,H
	jr Z,@positive_up_flag
	res 3,(ix+13)
	call CPL_HL    ;nbY es ahora positivo 
@positive_up_flag:
	ld (ix+10),HL
	ld b,(ix+13)
	ld a,(ix+14)
;who is larger
	ld hl,(ix+8)
	ld de,(ix+10)
	or a
	SBC HL, DE
	bit 7,H
	jr Z, @hl_geq_de
	SET 0,(ix+13)
	ld hl,(ix+8)
	LD (ix+8),de
	LD (ix+10),hl

@hl_geq_de:
	ld a,(ix+15)	;vdp command
	CP 01100000b	;search command
	JR Z,_isearch
_iline:
	;hay que indicar el registro de inicio , NO SIEMPRE ES EL 36
	;DEPENDERÁ DEL COMANDO
	call	wait_vdp_ready
	ld	a,36
	out	(VDP_CTRL_PORT),a   ;R#36
	ld	a,128+17
	out	(VDP_CTRL_PORT),a	;R#17 indirect access
	ld	c,VDP_INDR_PORT
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
	ld a,(ix+12)    ;Color
	out	(c),a		;       R#44   
	ld a,(ix+13)	;Flags
	out	(c),a       ;       R#45
	ld a,(ix+14)	;logop  
	ld l,(ix+15)	;vdp command
	or l
	out	(c),a		;       R#46
	pop ix
	ret

_isearch:
	call	wait_vdp_ready
	ld a,(ix+13)	;set Flag
	set 1,a
	ld (ix+13),a
	ld	a,32
	out	(VDP_CTRL_PORT),a
	ld	a,128+17
	out	(VDP_CTRL_PORT),a	;R#17 indirect access
	ld	c,VDP_INDR_PORT
	xor a
	ld hl,(ix)
	ld de,(ix+2)
	out	(c),l		;X from R#32
	out	(c),h		;       R#33
	out	(c),e		;Y from R#34
	out	(c),d		;       R#35
	ld	a,44
	out	(VDP_CTRL_PORT),a
	ld	a,128+17
	out	(VDP_CTRL_PORT),a	;R#17 indirect access
	ld a,(ix+12)    ;Color
	out	(c),a		;       R#44   
	ld a,(ix+13)	;Flags
	out	(c),a       ;       R#45
	ld a,(ix+14)	;logop  
	ld l,(ix+15)	;vdp command
	or l
	out	(c),a		;       R#46
	pop ix
	ret

	;E is the physical color
	;and the index in the palette table
ipalette:    
	;call	wait_vdp_ready i think we dont need that
	ld	a,E
	out	(VDP_CTRL_PORT),a
	ld	a,128+16
	out	(VDP_CTRL_PORT),a	   ;R#16 := pysical 
	ld	c,VDP_INDR_PORT
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

plotSinglePoint:
	push ix
	ld ix,StartX
	call	wait_vdp_ready
	ld	a,36
	out	(VDP_CTRL_PORT),a
	ld	a,128+17
	out	(VDP_CTRL_PORT),a	;R#17 := 36
	ld	c,VDP_INDR_PORT
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
chColors:
    OUT  (VDP_CTRL_PORT), A
	LD   A, 128+7
    OUT  (VDP_CTRL_PORT), A
    RET

wait_vdp_ready:
	
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
    jp c,wait_vdp_ready
    ret

read_vdp_status_register:
	di
    out (VDP_CTRL_PORT),a     ; select s#A
    ld a,15 + 128
    out (VDP_CTRL_PORT),a
    in a,(VDP_CTRL_PORT)
    ex af,af'
    xor a           ; ld a,0
    out (VDP_CTRL_PORT),a
    ld a,15 + 128
    ei
    out (VDP_CTRL_PORT),a
    ex af,af'
    ret


;-----------Palete copy in ram-----------------
;  A : logical color 
;  returns A: physical color
findPhysicalColor:
	LD B,15
	RLA
	RLA
	RLA
	RLA
	AND 11110000B
	LD E,A
	LD HL, PALETTE
@loop:
	LD A,(HL)
	AND 11110000B
	CP  E
	jr Z,@exit_loop
	INC HL
	INC HL
	DJNZ @loop
@exit_loop:
	LD A,15
	SUB B
	RET

;*******************TRIANGLE SECTION************************
; ==========================================================
; ZONA DE DATOS (RAM)
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


;
;PCSR	- Move cursor to specified position.
;   	  Inputs: DE = horizontal position (LHS=0)
;                 HL = vertical position (TOP=0)
; 	  Destroys: A,D,E,H,L,F
;
PCSR:
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
	RET
	

;*********************SCALE SUBRUTINES************************
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

;****************************SCROLL VIEWPOERT***********************
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
