    PUBLIC INIT_MAPPER_POINTERS
    PUBLIC GET_FREE_SEGMENT
    PUBLIC SELECT_SEGMENT_P2
    PUBLIC GET_CURRENT_SEGMENT_P2
    

    ;TENEMOS QUE DEFINIR ESTAS DOS DW EN EL CLIENTE DE LA LIBRERIA
    EXTERN MAPPER_JUMP_TABLE
    EXTERN MAIN_SEGMENT
;------------------------------------------------------------------------------------UTILITY
; --- DIRECCIONES MSX-DOS 2 MAPPER ---
; La dirección de la tabla de salto del Mapper se encuentra en &F24F


; Offset de la función ALL_SEG en la tabla (normalmente es el primer salto)
ALL_SEG_OFFSET    EQU 0 
PUT_P2_OFFSET     EQU 24h      ; Put Physical (Pon segmento A en página de HL)
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
    ; 1. Comprobar si existe la Extended BIOS
    LD   A, (MAIN_SEGMENT)
    OR  A
    RET NZ
    LD   A, (HOKVLD)
    BIT  0, A              ; El bit 0 debe ser 1
    JR   Z, NO_MAPPER    ; Si es 0, no hay soporte

    ; 2. Preparar llamada a EXTBIO
    LD   D, MAPPER_DEV_ID  ; D = 4 (Dispositivo Mapper)
    LD   E, MAPPER_FUNC_GET; E = 2 (Dame la dirección de la tabla)
    
    ; 3. Llamar
    CALL EXTBIO            ; Devuelve HL = Dirección base de la tabla
    
    ; 4. Verificar resultado (HL no debería ser 0)
    LD   A, H
    OR   L
    JR   Z, NO_MAPPER

    ; 5. Guardar el puntero para usarlo luego
    LD   (MAPPER_JUMP_TABLE), HL
    CALL    GET_CURRENT_SEGMENT_P2
    LD      (MAIN_SEGMENT),A
    OR   A                 ; Clear Carry (Éxito)
    RET

NO_MAPPER:
    XOR	A                  ; set a to 0 (No room error)
    SCF                    ; Set Carry (Error)
    RET

; ---------------------------------------------------------
; RUTINA: GET_FREE_SEGMENT
; Retorna: A = Segmento reservado, B = Slot
;          Carry Set = Error (No hay memoria)
; ---------------------------------------------------------
GET_FREE_SEGMENT:
    ; 1. Configurar parámetros para ALL_SEG
    XOR  A                  ; A = 0 (Pedimos RAM de Usuario)
    LD   B, A               ; B = 0 (En el Mapper Primario)
    
    ; 2. Preparar el salto
    LD   DE, ALL_SEG_OFFSET
    
    ; 3. Llamada al Mapper (Integrada para evitar líos de pila)
    LD   HL, (MAPPER_JUMP_TABLE)  
    ADD  HL, DE                 ; HL = F24F + 0
    
    ; 4. Ejecución
    ; Usamos JP (HL) para saltar a la tabla.
    ; La tabla salta a la rutina del DOS.
    ; La rutina del DOS hará un RET, que volverá a quien llamó a GET_FREE_SEGMENT.
    JP   (HL)

; -----------------------------------------------------------------------------
; RUTINA: SELECT_SEGMENT_P2
; -----------------------------------------------------------------------------
SELECT_SEGMENT_P2:
    PUSH AF
    LD   DE, PUT_P2_OFFSET
    LD   IX, (MAPPER_JUMP_TABLE) 
    ADD  IX, DE               ; IX ahora sí apunta a F24F + 18
    POP  AF
    JP   (IX)                 ; Salta al "JP xxxx" del sistema

; -----------------------------------------------------------------------------
; RUTINA: GET_CURRENT_SEGMENT_P2
; -----------------------------------------------------------------------------
GET_CURRENT_SEGMENT_P2:
    LD   DE, GET_P2_OFFSET
    LD   IX, (MAPPER_JUMP_TABLE) 
    ADD  IX, DE               ; IX apunta a F24F + 21
    JP   (IX)                 ; Salta al "JP xxxx" del sistema y retorna A
