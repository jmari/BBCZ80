EXTERN  VDU_CMD_W
KFUNC     EQU 0F87Fh   ;use key function working area on basic  to return the driver name
ORG  8000h


START_OF_DEVICE_HANDLER:
    JR     EXEC_VDU_FUNCTION            ; +0
    ; Offsets de funciones (1 byte cada una)
    DEVICE_ID:            DEFB 150       ; +2 
    DEVICE_NAME:         DEFM "BBC VDU Emulator v0.10", 0 
    DEVICE_VERSION:      DEFB 10        ;version 0.10 
EXEC_VDU_FUNCTION:
    PUSH AF
    LD   A, E               
    CP   1
    JR   Z, FUNC_VDU_OUT
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

FUNC_VDU_OUT:
    LD   E,L                              ; L is the char to output
    CALL VDU_CMD_W          
    INC SP                 ; Restore AF from the very beginning
    INC SP
    RET
END_OF_DEVICE_HANDLER: