    PUBLIC  INSTALL ; THIS IS THE COMMAND WE IMPLEMENT IN THIS MODULE
    PUBLIC  RESERVED_SEGMENTS
    PUBLIC  TOTAL_SEGMENTS
    PUBLIC  MAPPER_JUMP_TABLE
    PUBLIC  MAIN_SEGMENT
    PUBLIC  COPY_ACTIVE_CONTEXT_TO_BUFFER
    PUBLIC  COPY_BUFFER_TO_ACTIVE_CONTEXT
    PUBLIC  COPY_P2_TO_BUFFER
    PUBLIC  COPY_BUFFER_TO_P2
    PUBLIC  COPY_ACTIVE_CONTEXT_TO_P2
    PUBLIC  COPY_P2_TO_ACTIVE_CONTEXT
    PUBLIC  NEWLIN
    PUBLIC  XEQ
    PUBLIC  XEQ0
    PUBLIC  XEQ1
    PUBLIC  END

    EXTERN ALLOCATE_SEGMENT         ;MAPPER.ASM
    EXTERN SELECT_SEGMENT_P2        ;MAPPER.ASM
    EXTERN GET_CURRENT_SEGMENT_P2   ;MAPPER.ASM
    ;-------------------------------------------
    EXTERN  EXPRS   ; EVAL.ASM
    EXTERN  HIMEM   ; DATA.ASM
	EXTERN	RANDOM  ; DATA.ASM
	EXTERN	ERRTRP  ; DATA.ASM
	EXTERN	TRACEN  ; DATA.ASM
    EXTERN  TRAP    ; CMOS.ASM
    EXTERN  TLAST   ; EXEC.ASM
    EXTERN  CLOOP   ; MAIN.ASM
    EXTERN  OSSHUT  ; CMOS.ASM
    EXTERN  WARM    ; MAIN.ASM
    EXTERN  CLEAR   ; MAIN.ASM
    EXTERN  PBCDL   ; MAIN.ASM
    EXTERN  OUTCHR  ; MAIN.ASM
    EXTERN  OSCLI   ; CMOS.ASM
    ;----VARS
    EXTERN  DATPTR  ; DATA.ASM
    EXTERN  PAGE    ; DATA.ASM
    EXTERN  DSRCH   ; DSRCH
    EXTERN  CURLIN  ; DATA.ASM
    EXTERN  CMDTAB  ; EXEC.ASM
    ;---
    EXTERN	TELSE
	EXTERN  MELSE	;ELSE
	EXTERN	TWHEN
	EXTERN  WHEN	;WHEN
	EXTERN  TOTHERWISE
    ;----
 
    EXTERN  CR
    EXTERN  NXT
    EXTERN  LET0
    EXTERN  TCMD
    EXTERN  EXTRAS
    EXTERN  ERROR
    EXTERN  LOAD0

    ;-----DATA MODULE
    EXTERN  ACCS
    EXTERN  USER



START_OF_P2 EQU 8000h



;----------------COPIA DEL CONTEXTO PRINCIPAL 1KBYTE ---------------
CONTEXT_COPY: DEFS 1028
MAPPER_JUMP_TABLE:  DEFW 0
MAIN_SEGMENT:       DEFB 0
TOTAL_SEGMENTS:     DEFB 0                ; Cuántos segmentos he pedido
SEGMENT_COUNTER:    DEFB 0                ; helper byte for loops
RESERVED_SEGMENTS:  DEFS 249,0      ; Reserve ten segments, asumming 256 segments (max 4096 bytes in RAM)
                                    ; msxdos has almost 6 ram segments reserved and VDU ext has one


; Register the segment as reserved by us
; inputs: A, segment number
; output: A, segment number
; destroys AF'
REG_SEGMENT:
    ; Guardamos el segmento en nuestra lista
    EX   AF,AF'
    LD    HL, RESERVED_SEGMENTS
    LD    A, (TOTAL_SEGMENTS)
    LD    E, A
    LD    D, 0
    ADD   HL, DE            ; HL = LISTA_SEGS + TOTAL_SEGMENTS
    INC   A
    LD    (TOTAL_SEGMENTS), A
    LD    (HL), A           ; Guardamos el ID en el array
    EX   AF,AF'            ; Do not destroy AF it has the segment number
    RET
COPY_ACTIVE_CONTEXT_TO_BUFFER:
    LD  HL, ACCS
    LD  DE, CONTEXT_COPY
    LD  BC, USER-ACCS
    LDIR
    RET 
COPY_P2_TO_BUFFER:
    LD  HL, START_OF_P2
    LD  DE, CONTEXT_COPY
    LD  BC, USER-ACCS
    LDIR
    RET 
COPY_BUFFER_TO_P2:
    LD  HL, CONTEXT_COPY
    LD  DE, START_OF_P2
    LD  BC, USER-ACCS
    LDIR
    RET 
COPY_BUFFER_TO_ACTIVE_CONTEXT:
    LD  HL, CONTEXT_COPY
    LD  DE, ACCS
    LD  BC, USER-ACCS
    LDIR
    RET 
COPY_ACTIVE_CONTEXT_TO_P2:
    LD  HL, ACCS
    LD  DE, START_OF_P2
    LD  BC, USER-ACCS
    LDIR
    RET 
COPY_P2_TO_ACTIVE_CONTEXT:
    LD  HL, START_OF_P2
    LD  DE, ACCS
    LD  BC, USER-ACCS
    LDIR
    RET 


INITIALIZE_SEGMENT_CONTEXT:
    LD  IX,(HIMEM)
    LD  (START_OF_P2 + USER - ACCS + 1),IX ;Guarda HIMEM del contexto al final de la tabla (+1 y +2)
    LD  IY,START_OF_P2 + USER - ACCS +3
    LD  (CURLIN),IY
    LD  (PAGE),IY ; CARGAMOS EL PROGRAMA A PARTIR DEL INICIO DE LA PAGINA 
    LD  IX,2
    ADD IX,SP
    LD (HIMEM),IX  ;nuevo HIMEM es el SP del contexto anterior
    RET
RESTORE_HIMEM:
 
    LD  IX,(HIMEM)  ;nuevo HIMEM es el SP del contexto anterior
    LD  SP,IX
    LD  IX, (START_OF_P2 + USER - ACCS + 1) ;recuperamos HIMEM del contexto anterior
    LD (HIMEM),IX  ;nuevo HIMEM es el SP del contexto anterior
    JP BACK_TO_P2 

; =============================================================================
; COMANDO INSTALL: Clon funcional de CHAIN para inicializar el sistema de carga
; =============================================================================



    ;------------------FALTAN LOS PROC Y FUNC Y DYNVARS...................
INSTALL:
    ; --- PARTE 1: Procesamiento del nombre de archivo ---
    CALL    EXPRS       ; Evalúa la expresión (el nombre del archivo tras INSTALL)
    LD      A,CR        ; Carga el retorno de carro
    LD      (DE),A      ; Termina la cadena del nombre del archivo en memoria
    PUSH    IY
    ;CHECK_DOS_VERSION:
    ;LD   C, 6Fh          ; Función _DOSVER de MSX-DOS 2
    ;CALL 0F37Dh           ; Llamada al BDOS
    ;LD   A, B            ; B devuelve la versión principal (Major version)
    ;CP   2
    ;JP   C, ERROR      ; Si es menor que 2, es DOS 1

    CALL GET_CURRENT_SEGMENT_P2
    PUSH AF             ;Guarda el segmento activo en la pila
    XOR A               ;solicitamos RAM DE USUARIO
    CALL ALLOCATE_SEGMENT
    ; A = Segmento, B = Slot (o Carry si error)
    JP  C,_ERROR ;no room
    ; --- SELECCIONA UN SEGMENTO DE 16k LIBRE ---
    CALL REG_SEGMENT
    ;ENTRADA: A = Número de segmento (0-255)
SWAP_SEGMENT:
    CALL SELECT_SEGMENT_P2  
    CALL COPY_ACTIVE_CONTEXT_TO_P2
    ; --- PARTE 2: Limpieza de Pila y Carga ---  
INIT_CONTEXT:
    CALL INITIALIZE_SEGMENT_CONTEXT
    CALL LOAD0       ; Carga el archivo desde disco/dispositivo a la dirección PAGE
    ;JR force_fin
    ; --- PARTE 4: El punto crítico del Heap ---
    CALL    CLEAR       ; BORRA DYNVAR, LOMEM y VARTOP  PREPARA el nuevo contexto
    ; --- PARTE 5: Preparación de punteros de datos y programa ---
    LD      HL,0
    LD      (ERRTRP),HL ; Desactiva trampas de error actuales
    LD      HL,(PAGE)
    CALL    DSRCH       ; Escanea el código cargado buscando sentencias "DATA"
    LD      (DATPTR),HL ; Guarda el puntero al primer dato disponible
; --- BUCLE PRINCIPAL DE EJECUCIÓN (XEQ0) ---
XEQ0:
    CALL    NEWLIN      ; Procesa número de línea, longitud y chequea TRACE
    LD      A,(IY)      ; Lee el primer token/carácter de la línea
    CP      TELSE
    JP      Z,MELSE     ; Salta si es ELSE
    CP      TWHEN
    JP      Z,WHEN      ; Salta si es WHEN
    CP      TOTHERWISE
    JP      Z,WHEN
XEQ:
    LD      (CURLIN),IY ; Guarda la línea actual para posibles mensajes de error
    CALL    TRAP        ; Comprueba si el usuario pulsó ESC (Keyboard break)

XEQ1:
    CALL    NXT         ; Obtiene el siguiente carácter/token saltando espacios
    INC     IY          ; Avanza el puntero de ejecución
    CP      ':'         ; ¿Hay más de una sentencia en la misma línea?
    JR      Z,XEQ1
    CP      CR          ; ¿Fin de línea (Carriage Return)?
    JR      Z,XEQ0      ; Salta a la siguiente línea del programa

    ; --- PARTE 6: Decodificación de Comandos ---
    CP      TLAST       ; Comprueba si es un token de comando o una asignación
    JP      PE,LET0     ; Si no es token, asume que es un LET implícito (ej: A=1)
    
    SUB     TCMD        ; Resta el offset de la tabla de comandos
    JP      M,EXTRAS    ; Si es menor, busca en tabla de comandos extra
    
    ADD     A,A         ; Multiplica por 2 (cada entrada en CMDTAB es un DW)
    LD      C,A
    LD      B,0
    LD      HL,CMDTAB   ; HL apunta a la base de la tabla de saltos
    ADD     HL,BC
    
    LD      A,(HL)      ; Lee la dirección de la rutina del comando (LSB)
    INC     HL
    LD      H,(HL)      ; Lee la dirección (MSB)
    LD      L,A
    
    CALL    NXT         ; Prepara el siguiente token para la rutina del comando
    JP      (HL)        ; SALTO FINAL: Ejecuta la rutina (PRINT, FOR, GOTO, etc.)
                        ; ¡como controlamos que vuelva aqui y no al WARM!!
;
;END
;

ENDIM:	
    CALL GET_CURRENT_SEGMENT_P2
   ; IF ERROR OR ESC WE ARE HERE WE CAN NOT CHANGE CONTEXT..
   ; WE SHOULD EDIT AND FIX THE PROBLEM...SAVE AND COM BACK USING END
    PUSH	IY
	POP	HL
	LD	BC,(PAGE)
	SBC	HL,BC		;IMMEDIATE MODE ?
	JP	C,CLOOP
END:
    CALL GET_CURRENT_SEGMENT_P2
    LD	HL,(MAIN_SEGMENT)   ;IF MAIN SEGMENT GO TO WARM ELSE SWITCH BACK CONTEXT
    CP L
    JR NZ,SWAP_CONTEXT
    LD	E,0
	CALL	OSSHUT		;CLOSE ALL FILES
	JP	WARM		;"Ready"
;
NEWLIN:	LD	A,(IY+0)	;A=LINE LENGTH
	LD	BC,3
	ADD	IY,BC
	OR	A
	JR	Z,ENDIM		;LENGTH=0, EXIT
	LD	HL,(TRACEN)
	LD	A,H
	OR	L
	RET	Z
	LD	D,(IY-1)	;DE = LINE NUMBER
	LD	E,(IY-2)
	SBC	HL,DE
	RET	C
	EX	DE,HL
	LD	A,'['		;TRACE
	CALL	OUTCHR
	CALL	PBCDL
	LD	A,']'
	CALL	OUTCHR
	LD	A,' '
	JP	OUTCHR
; --- FINALIZACIÓN ---
;
;ROUTINES FOR EACH STATEMENT:
;
;OSCLI
;
    CALL	EXPRS
	LD	A,CR
	LD	(DE),A
	LD	HL,ACCS
	CALL	OSCLI
	JP	XEQ
SWAP_CONTEXT:
    JP RESTORE_HIMEM   
BACK_TO_P2:
    CALL COPY_ACTIVE_CONTEXT_TO_BUFFER   ;EN BUFFER ESTA EL CONTEXTO ACTIVO
    CALL COPY_P2_TO_ACTIVE_CONTEXT       ;EN P2 ESTABA EL CONTEXTO ANTERIOR, LO CARGAMOS DE NUEVO
    CALL COPY_BUFFER_TO_P2
               ; Por algún motivo la pila tiene un word de más
    POP AF                   ;recupera el segmento activo
    CALL SELECT_SEGMENT_P2
    POP IY
    INC IY
    JP	XEQ0

_ERROR:
    XOR	A   ;No Room!
    JP ERROR


; Inputs: A is the segment where the library is installed in
ACTIVATE_CONTEXT:
	CALL SELECT_SEGMENT_P2
	CALL COPY_P2_TO_BUFFER       ;EN P2 ESTABA EL CONTEXTO ANTERIOR, LO CARGAMOS DE NUEVO
	CALL COPY_ACTIVE_CONTEXT_TO_P2   ;EN BUFFER ESTA EL CONTEXTO ACTIVO
    CALL COPY_BUFFER_TO_ACTIVE_CONTEXT       ;EN P2 ESTABA EL CONTEXTO ANTERIOR, LO CARGAMOS DE NUEVO
	