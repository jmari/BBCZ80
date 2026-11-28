    PUBLIC INIT_MAPPER_POINTERS
    PUBLIC ALLOCATE_SEGMENT
    PUBLIC SELECT_SEGMENT_P1
    PUBLIC GET_CURRENT_SEGMENT_P1
    PUBLIC SELECT_SEGMENT_P2
    PUBLIC GET_CURRENT_SEGMENT_P2
    

    ;TENEMOS QUE DEFINIR ESTAS DOS DW EN EL CLIENTE DE LA LIBRERIA
    EXTERN MAPPER_JUMP_TABLE
    EXTERN P1_SEGMENT
    EXTERN MAIN_SEGMENT
;------------------------------------------------------------------------------------UTILITY
; --- DIRECCIONES MSX-DOS 2 MAPPER ---
; La dirección de la tabla de salto del Mapper se encuentra en &F24F


; Offset de la función ALL_SEG en la tabla (normalmente es el primer salto)
ALL_SEG_OFFSET    EQU 0 
PUT_P1_OFFSET     EQU 1Eh      ; Put Physical (Pon segmento A en página de HL)
PUT_P2_OFFSET     EQU 24h      ; Put Physical (Pon segmento A en página de HL)

GET_P1_OFFSET     EQU 21h      ; Get Physical (Lee segmento de página de HL en A)
GET_P2_OFFSET     EQU 27h      ; Get Physical (Lee segmento de página de HL en A)



; --- DEFINICIONES ---
HOKVLD           EQU 0FB20h    ; Flag de validación de Hooks/Extended BIOS
EXTBIO           EQU 0FFCAh    ; Vector de salto a la Extended BIOS
MAPPER_DEV_ID    EQU 4         ; ID del dispositivo Mapper en EXTBIO
MAPPER_FUNC_GET  EQU 2         ; Función: Obtener dirección de la tabla

; Variable en RAM para guardar la dirección REAL de la tabla
; Inicialízala a 0. Si sigue siendo 0 tras init, es que hubo error.       

; -----------------------------------------------------------------------------
; RUTINA: INIT_MAPPER_POINTERS
; ACCION: Pregunta a la BIOS dónde está la tabla de saltos del Mapper
; SALIDA: Rellena (REAL_MAPPER_ADDR) y Carry=0 si OK. Carry=1 si error.
; -----------------------------------------------------------------------------
CSRSW EQU 0FCA9h
INIT_MAPPER_POINTERS:
    LD   A, 1
    LD   (CSRSW), A

    ; 1. Verificar si ya está inicializado mirando el puntero (no MAIN_SEGMENT)
    LD   HL, (MAPPER_JUMP_TABLE)
    LD   A, H
    OR   L
    RET  NZ                  ; Ya inicializado (OR L deja Carry = 0)

    ; 2. Comprobar Extended BIOS
    LD   A, (HOKVLD)
    BIT  0, A
    JR   Z, NO_MAPPER

    ; 3. Llamar a EXTBIO con A = 0 obligatorio
    XOR  A                   ; A = 0 requerido por la especificación MSX-DOS 2
    LD   HL, 0000h
    LD   D, MAPPER_DEV_ID    ; D = 4
    LD   E, MAPPER_FUNC_GET  ; E = 2
    CALL EXTBIO

    ; 4. Validar respuesta (A = total segmentos != 0, HL = tabla != 0)
    OR   A
    JR   Z, NO_MAPPER
    LD   A, H
    OR   L
    JR   Z, NO_MAPPER

    ; 5. Guardar tabla y segmento inicial
    LD   (MAPPER_JUMP_TABLE), HL
    CALL GET_CURRENT_SEGMENT_P2
    LD   (MAIN_SEGMENT), A
    OR   A                   ; Carry = 0
    RET

NO_MAPPER:
    XOR  A
    SCF                      ; Carry = 1
    RET



; ---------------------------------------------------------
; RUTINA: ALLOCATE_SEGMENT
; Retorna: A = Segmento reservado, B = Slot
;          Carry Set = Error (No hay memoria)
; ---------------------------------------------------------
ALLOCATE_SEGMENT:
    ; 1. Configurar parámetros para ALL_SEG              
    LD   B, 0       ; B = 0 (En el Mapper Primario)
    ; 2. Preparar el salto
    LD   DE, ALL_SEG_OFFSET
    ; 3. Llamada al Mapper (Integrada para evitar líos de pila)
    LD   HL, (MAPPER_JUMP_TABLE)  
    ADD  HL, DE                 ; HL = F24F + 0
    ; 4. Ejecución
    ; Usamos JP (HL) para saltar a la tabla.
    ; La tabla salta a la rutina del DOS.
    ; La rutina del DOS hará un RET, que volverá a quien llamó a ALLOCATE_SEGMENT.
    JP   (HL)



; --- SELECTORES (preservan TODOS los registros, incluido AF, DE, HL, IX) ---

SELECT_SEGMENT_P1:
    PUSH HL
    PUSH DE
    PUSH AF
    LD   HL, (MAPPER_JUMP_TABLE)
    LD   DE, PUT_P1_OFFSET
    ADD  HL, DE
    POP  AF
    POP  DE
    EX   (SP), HL
    RET

SELECT_SEGMENT_P2:
    PUSH HL
    PUSH DE
    PUSH AF
    LD   HL, (MAPPER_JUMP_TABLE)
    LD   DE, PUT_P2_OFFSET
    ADD  HL, DE
    POP  AF
    POP  DE
    EX   (SP), HL
    RET

; --- GETTERS (devuelven segmento en A, preservan BC, DE, HL, IX, IY) ---
GET_CURRENT_SEGMENT_P1:
    PUSH HL
    PUSH DE
    LD   HL, (MAPPER_JUMP_TABLE)
    LD   DE, GET_P1_OFFSET
    ADD  HL, DE
    POP  DE
    EX   (SP), HL
    RET

GET_CURRENT_SEGMENT_P2:
    PUSH HL
    PUSH DE
    LD   HL, (MAPPER_JUMP_TABLE)
    LD   DE, GET_P2_OFFSET
    ADD  HL, DE
    POP  DE
    EX   (SP), HL
    RET