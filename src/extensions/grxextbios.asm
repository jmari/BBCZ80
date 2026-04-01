EXTERN  VDU_CMD_W
ORG  8000h


START_OF_DEVICE_HANDLER:
    JR     EXEC_VDU_FUNCTION            ; +0
    ; Offsets de funciones (1 byte cada una)
    DEVICEID:            DEFB 150       ; +2 
    OFF_FUNC_0:          DEFB FUNC_PRESENCE - START_OF_DEVICE_HANDLER ; +3
    OFF_FUNC_1:          DEFB FUNC_VDU_OUT  - START_OF_DEVICE_HANDLER ; +4
    ; --- TABLA DE DATOS 
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


; --- FUNCIONES ---

FUNC_PRESENCE:
    POP  AF      ;CLEAN THE PILE
    LD   A, 255
    RET

FUNC_VDU_OUT:
    LD   E,L                              ; L is the char to output
    CALL VDU_CMD_W          
    POP  AF                 ; Restore AF from the very beginning
    RET
END_OF_DEVICE_HANDLER: