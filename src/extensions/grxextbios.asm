EXTERN  VDU_CMD_W
ORG  8000h

DEVICEID EQU 150
PUT_P2_OFFSET     EQU 24h
GET_P2_OFFSET     EQU 27h

START_OF_DEVICE_HANDLER:
    JR     DEVICE_VDU_HANDLER            ; +0
    ; --- TABLA DE DATOS (El cargador escribe aquí) ---
    MAPPER_JUMP_TABLE:   DEFW 0000h      ; +2
    MAIN_SEGMENT:        DEFB 00h        ; +4
    DEVICE_RAM_SEGMENT:  DEFB 00h        ; +5
    OLD_STUB_CODE:       DEFM 0C9h,0C9h,0C9h,0C9h,0C9h          ; +6 (Old STUB)
    ; Offsets de funciones (1 byte cada una)
    MY_ADDRESS:          DEFW 0000h      ;  ; +11
    DRV_SIZE:            DEFW END_OF_DEVICE_HANDLER-START_OF_DEVICE_HANDLER; +13
    OFF_FUNC_0:          DEFB FUNC_PRESENCE - START_OF_DEVICE_HANDLER ; +15
    OFF_FUNC_1:          DEFB FUNC_VDU_OUT  - START_OF_DEVICE_HANDLER ; +16
     
PATCH_DRIVER:
    PUSH DE
    LD DE, (MY_ADDRESS)
    LD (_PATCH2+2), DE
    LD (_PATCH3+2), DE
    LD (_PATCH4+2), DE
    LD (_PATCH5+2), DE
    POP DE
    RET

DEVICE_VDU_HANDLER:
    PUSH BC
    PUSH IX
    PUSH AF
    LD   A, D
    CP   DEVICEID
    JR   NZ, NEXT_DEVICE               
    JR   EXEC_VDU_FUNCTION  

NEXT_DEVICE:
    POP  AF                 ; Restore AF from the very beginning
    POP  IX
    POP  BC
    JR OLD_STUB_CODE

EXEC_VDU_FUNCTION:
_PATCH2:
    LD   IX, MY_ADDRESS   ; Cargamos nuestra ubicación real
    
    LD   A, E               
    CP   1
    JR   Z, GET_FUNC_1
    CP   0
    JR   Z, GET_FUNC_0
    JR   NO_FN

GET_FUNC_1:
    LD   A, (IX+16)         ; Leer offset de Función 1
    JR   DO_DISPATCH
GET_FUNC_0:
    LD   A, (IX+15)         ; Leer offset de Función 0

DO_DISPATCH:
    LD   C, A
    LD   B, 0
    ADD  IX, BC             ; IX = MY_ADDRESS + Offset
    JP   (IX)               ; Salto a la función (el RET de la función volverá al CALLer)

NO_FN:
    POP  AF                 ; Restore AF from the very beginning
    POP  IX
    POP  BC
    RET


; --- FUNCIONES ---

FUNC_PRESENCE:
    POP  AF      ;CLEAN THE PILE
    LD   A, 255
    POP  IX
    POP  BC
    RET


FUNC_VDU_OUT:
    
    CALL GET_CURRENT_SEGMENT_P2
    PUSH AF                 ; Guardar segmento original de P2
_PATCH3:    
    LD   IX, MY_ADDRESS
    LD   A, (IX+5)          ; Leer DEVICE_RAM_SEGMENT desde offset +5
    
    CALL SELECT_SEGMENT_P2    
    LD   E,L
    CALL VDU_CMD_W          
    
    POP  AF                 ; Recuperar segmento original
    CALL SELECT_SEGMENT_P2 
    JR NO_FN                ;CLEAN UP

; --- RUTINAS MAPPER ---

SELECT_SEGMENT_P2:
    PUSH AF
    LD   DE, PUT_P2_OFFSET
_PATCH4:
    LD   IX, MY_ADDRESS
    LD   IX, (IX+2)         ; IX = MAPPER_JUMP_TABLE (+2)
    ADD  IX, DE
    POP  AF
    JP   (IX)

GET_CURRENT_SEGMENT_P2:
    LD   DE, GET_P2_OFFSET
_PATCH5:
    LD   IX, MY_ADDRESS
    LD   IX, (IX+2)
    ADD  IX, DE
    JP   (IX)

END_OF_DEVICE_HANDLER: