EXTERN  VDU_CMD_W
KFUNC     EQU 0F88Fh   ;use key function working area on basic  to return the driver name
ORG  4000h


START_OF_DEVICE_HANDLER:
    JR     EXEC_SCC_FUNCTION            ; +0
    ; Offsets de funciones (1 byte cada una)
    DEVICE_ID:           DEFB 151       ; +2 
    DEVICE_NAME:         DEFM "SCC Driver", 0 
    DEVICE_VERSION:      DEFB 01        ;version 0.01 
EXEC_SCC_FUNCTION:
    PUSH AF
    LD   A, E               
    CP   1
    JR   Z, FUNC_WRITE_SCC  
    CP   0
    JR   Z, FUNC_PRESENCE
    JR   NO_FN

NO_FN:
    POP  AF                 ; Restore AF from the very beginning
    RET
COPY_STR:
    LD   A, (HL)
    LDI                 ; Copia (HL)->(DE), HL++, DE++, BC--
    AND  A              ; ¿Era el byte cero?
    JR   NZ, COPY_STR
    RET

; --- FUNCIONES ---

FUNC_PRESENCE:
    POP  AF      ;CLEAN THE PILE
    LD   HL, DEVICE_NAME
    LD   DE, KFUNC
    CALL COPY_STR
    LD   A, (DEVICE_ID)
    LD   BC, (DEVICE_VERSION)
    RET




; --------------------------------------------------------------
; RUTINA DE ESCRITURA EN SCC (Invocada desde el Payload)
; Al estar en P3, sobrevive aunque cambiemos el slot de P2.
; ENTRADA: HL = Dirección SCC (9xxx), A = Valor a escribir
; --------------------------------------------------------------
FUNC_WRITE_SCC:
    PUSH AF
    PUSH HL
    ; 1. Leer slot actual de P2 (del puerto A8h y variables extendidas)
    ; (Simplificado: asumimos macro que lee el slot actual de P2 en C)
    CALL GET_CURRENT_SLOT_P2
    LD   (SAVED_SLOT_P2), A

    ; 2. Poner Slot del SCC en la Página 2
    LD   A, (SCC_SLOT_ID)
    LD   H, 080h            ; Página 2
    CALL ENASLT             ; Rutina BIOS (024h) o macro equivalente de P3

    ; 3. ESCRITURA EN EL CHIP SCC (El chip ya es visible en 9800h+)
    POP  HL
    POP  AF
    LD   (HL), A            ; <-- LA ESCRITURA REAL AL SCC

    ; 4. Restaurar Slot original de P2
    PUSH AF
    LD   A, (SAVED_SLOT_P2)
    LD   H, 080h
    CALL ENASLT
    POP  AF
    RET

SCC_SLOT_ID:   DB 0         ; Slot donde se encontró el SCC
SAVED_SLOT_P2: DB 0
END_OF_DEVICE_HANDLER: