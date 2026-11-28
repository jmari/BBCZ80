    PUBLIC MAPPER_JUMP_TABLE
    PUBLIC MAIN_SEGMENT
    EXTERN INIT_MAPPER_POINTERS
    EXTERN ALLOCATE_SEGMENT
    EXTERN SELECT_SEGMENT_P1
    EXTERN GET_CURRENT_SEGMENT_P1


; --- CONSTANTS ---
P01             EQU 4000h
MAXBYTES        EQU 4000h 
TRAMPOLINE_DEST EQU  0F41FH  ; KBUF (F41FH, 318)
                            ; contents:	buffer to store characters typed; where direct statements
		                    ; are stored in ASCII code...we use to keep the trampolines
EXTBIOS_HOOK    EQU  0FFCAh  ; System Hook for Extended BIOS (ADDRESS) 

; --- MSX-DOS 2 FUNCTIONS ---
_OPEN           EQU 43h
_READ           EQU 48h
_CLOSE          EQU 45h
_TERM           EQU 00h
DOS2            EQU 0005h    


; --- DRIVER HEADER OFFSETS  ---
DEVICE_ID       EQU 4002h      ; DEVICE ID 

CALLS           EQU 0Fh      ; Interslot call

    ORG 100H
    JP DRIVER_LOAD

;------------------------TRAMPOLINE DEFINITION---------------------------------
    ; D is device ID
    ; E is function ID
; --- CONSTANTS ADICIONALES PARA CONTADOR SEGURO ---
DRV_MAGIC_ADDR          EQU 0F55Ah  ; 2 bytes para firma 'DR'
DRV_MAGIC_VALUE         EQU 5244h   ; 'D' (44h), 'R' (52h) en little-endian
NUMBER_OF_ALLOCATED_DRV EQU 0F55Ch  ; 1 byte: número de trampolines instalados
MAX_TRAMPOLINES         EQU 10      ; 10 * ~33 bytes cabe de sobra en KBUF (318 bytes)

;------------------------TRAMPOLINE DEFINITION---------------------------------
TRAMPOLINE: 
    JR   START_TRAMPOLINE
OLD_STUB_CODE:
    DEFB 0C9h, 0C9h, 0C9h, 0C9h, 0C9h       ; 5 bytes para encadenar hook anterior
START_TRAMPOLINE:
    PUSH IX
    PUSH IY
    PUSH DE
    PUSH AF
    LD   A, D
_PATCH_DEVICEID:
    CP   00h
    JR   NZ, NEXT_DEVICE  
    POP  AF
    POP  DE                 ; D = device ID, E = function ID
_PATCH_MAPPERTABLE:
    CALL 0000h              ; Parcheado a MAPPER_JUMP_TABLE + 0Fh (CALLS)
_PATCH_SEGMENT:
    DB   03h                ; Parcheado con DEVICE_RAM_SEGMENT
    DW   P01                ; Destino: 4000h (Página 1)
    POP  IY                 ; <-- IMPORTANTE: Restaurar IY después de CALLS
    POP  IX
    RET
NEXT_DEVICE:
    POP  AF
    POP  DE
    POP  IY
    POP  IX
    JR   OLD_STUB_CODE
END_OF_TRAMPOLINE:
;-----------------------------END OF TRAMPOLINE DEFINITION---------------------------



;-----------------------------START OF DRIVER LOADER---------------------------------
; --- MAPPER SUPPORT DATA ---
MAPPER_JUMP_TABLE:  DEFW 0000h
MAIN_SEGMENT:       DEFB 00h
DEVICE_RAM_SEGMENT: DEFB 00h

; --- DATA STORAGE ---
DRIVER_FILE_NAME:   DEFS 64, 0   ; Buffer for ASCIIZ filename
                    DEFM  13, 10, "$"
FILE_HANDLE:        DEFB 0

; --- MESSAGES ---
ERR_USAGE_MSG:      DEFM "Usage: LD <filename.lib>", 13, 10, "$"
OK_MSG:             DEFM "Driver successfully loaded at Page 2", 13, 10, "$"
ERR_OPEN_MSG:       DEFM "Error: File not found", 13, 10, "$"
ERR_READ_MSG:       DEFM "Error: Disk read failure", 13, 10, "$"
ERR_BIOS_MSG:       DEFM "Error: No EXTBIOS support found", 13, 10, "$"
ERR_MAPPER_MSG:     DEFM "Error: No Mapper support found", 13, 10, "$"

; --- COMMAND LINE PARSER ---
PARSE_COMMAND_LINE:
    LD   A, (0080h)         ; Read argument length
    OR   A                  
    JR   Z, NO_ARGS_ERROR   

    LD   B, A               ; B = char counter
    LD   HL, 0081h          ; HL = start of arguments

SKIP_SPACES:
    LD   A, (HL)
    CP   ' '                
    JR   NZ, COPY_FILENAME  
    INC  HL
    DJNZ SKIP_SPACES        

    JR   NO_ARGS_ERROR      

COPY_FILENAME:
    LD   DE, DRIVER_FILE_NAME 

COPY_LOOP:
    LD   A, (HL)
    CP   ' '                ; Space marks end of argument
    JR   Z, END_FILENAME
    CP   13                 ; Carriage Return marks end of line
    JR   Z, END_FILENAME
    CP   0                  
    JR   Z, END_FILENAME

    LD   (DE), A            
    INC  HL
    INC  DE
    DJNZ COPY_LOOP          

END_FILENAME:
    XOR  A                  
    LD   (DE), A            ; MSX-DOS 2 needs ASCIIZ (null terminator)
    RET

NO_ARGS_ERROR:
    LD   DE, ERR_USAGE_MSG
    LD   C, 09h             
    CALL DOS2
    LD   C, 00h             ; Terminate program
    CALL DOS2
    RET

; --- MAIN LOADER LOGIC ---
DRIVER_LOAD:
    CALL PARSE_COMMAND_LINE
    
    ; Inicializar Mapper
    CALL INIT_MAPPER_POINTERS 
    LD   DE, ERR_BIOS_MSG
    LD   B, 0DFh
    JP   C, REPORT_AND_EXIT
    
    ; Guardar segmento original de Página 1 en la pila
    CALL GET_CURRENT_SEGMENT_P1
    PUSH AF                 

    ; Reservar 1 segmento de sistema (A=1)
    LD   A, 1
    CALL ALLOCATE_SEGMENT
    LD   DE, ERR_MAPPER_MSG
    LD   B, 0DFh
    JP   C, FATAL_ERROR
    LD   (DEVICE_RAM_SEGMENT), A 

    ; Mapear nuevo segmento en Página 1 (4000h-7FFFh)
    CALL SELECT_SEGMENT_P1  
    
    ; Cargar el archivo en 4000h (si falla, vuelve con Carry=1, DE=msg, B=error)
    CALL LOAD0       
    JP   C, FATAL_ERROR  

    ; --- COPIAR HOOK ACTUAL (5 BYTES) EN OLD_STUB_CODE (Z80 PURO) ---
    LD   HL, EXTBIOS_HOOK
    LD   DE, OLD_STUB_CODE
    LD   BC, 5
    LDIR

    ; --- PARCHEAR TRAMPOLINE (Z80 PURO) ---
    LD   HL, (MAPPER_JUMP_TABLE)
    LD   BC, CALLS                          ; 000Fh
    ADD  HL, BC
    LD   (_PATCH_MAPPERTABLE + 1), HL       ; Dirección de CALLS

    LD   A, (DEVICE_ID)                     ; Lee ID del driver en 4002h
    LD   (_PATCH_DEVICEID + 1), A

    LD   A, (DEVICE_RAM_SEGMENT)
    LD   (_PATCH_SEGMENT), A

    ; --- INICIALIZAR FIRMA Y CALCULAR DESTINO EN KBUF ---
    LD   HL, (DRV_MAGIC_ADDR)
    LD   DE, DRV_MAGIC_VALUE
    OR   A
    SBC  HL, DE
    JR   Z, MAGIC_OK                        ; Ya había drivers cargados antes
    LD   HL, DRV_MAGIC_VALUE
    LD   (DRV_MAGIC_ADDR), HL
    XOR  A
    LD   (NUMBER_OF_ALLOCATED_DRV), A       ; Primer driver = índice 0

MAGIC_OK:
    LD   A, (NUMBER_OF_ALLOCATED_DRV)
    CP   MAX_TRAMPOLINES
    LD   DE, ERR_MAPPER_MSG                 ; O un mensaje específico de tabla llena
    LD   B, 0DFh
    JP   NC, FATAL_ERROR

    ; HL = TRAMPOLINE_DEST + (A * tamaño_trampolín)
    LD   HL, TRAMPOLINE_DEST
    LD   DE, END_OF_TRAMPOLINE - TRAMPOLINE
    OR   A
    JR   Z, DEST_READY
    LD   B, A
CALC_DEST_LOOP:
    ADD  HL, DE
    DJNZ CALC_DEST_LOOP

DEST_READY:
    EX   DE, HL                             ; DE = dirección destino del nuevo trampolín

    ; --- COPIAR TRAMPOLÍN Y ACTUALIZAR HOOK CON INTERRUPCIONES DESACTIVADAS ---
    DI
    ; Primero copiamos el trampolín a su sitio definitivo (DE)
    PUSH DE                                 ; Guardamos inicio del trampolín para el Hook
    LD   HL, TRAMPOLINE
    LD   BC, END_OF_TRAMPOLINE - TRAMPOLINE
    LDIR                                    ; Copia el trampolín ANTES de activar el hook
    POP  HL                                 ; HL = dirección donde quedó el trampolín

    ; Ahora apuntamos EXTBIOS_HOOK al trampolín ya copiado
    LD   A, 0C3h                            ; Opcode JP nn
    LD   (EXTBIOS_HOOK), A
    LD   (EXTBIOS_HOOK + 1), HL

    ; Incrementar contador de trampolines (1 byte)
    LD   HL, NUMBER_OF_ALLOCATED_DRV
    INC  (HL)

    ; --- RESTAURAR SEGMENTO ORIGINAL DE PÁGINA 1 Y SALIR ---
    POP  AF                                 ; A = segmento original de P1
    CALL SELECT_SEGMENT_P1
    EI

    LD   DE, OK_MSG
    LD   C, 09h
    CALL DOS2
    LD   B, 0
    LD   C, 62h                             ; _TERM
    CALL DOS2

FATAL_ERROR:
    ; Espera en la pila el AF del segmento original, DE = mensaje ($), B = código DOS2
    POP  AF
    CALL SELECT_SEGMENT_P1                  ; Restaura P1 SIEMPRE antes de llamar a DOS2
    EI
    JR   REPORT_AND_EXIT

; --- DISK LOADING ROUTINE (Retorna con Carry=1 en caso de error, NO sale directo) ---
LOAD0:  
    LD   DE, DRIVER_FILE_NAME
    XOR  A                                  ; Read-only
    LD   C, _OPEN
    CALL DOS2
    OR   A               
    JR   NZ, ERROR_OPEN
    LD   A, B
    LD   (FILE_HANDLE), A

    LD   DE, P01                            ; 4000h
    LD   HL, MAXBYTES                       ; 4000h (16 KB)
    LD   C, _READ
    CALL DOS2
    ; Nota: En MSX-DOS 2, _READ devuelve error EOF (0C7h) si el fichero mide < 16KB
    ; pero ha leído todos los bytes bien en HL. Si tu driver mide menos de 16KB:
    OR   A
    JR   Z, READ_OK
    CP   0C7h                               ; _EOF no es error si HL > 0
    JR   NZ, ERROR_READ
    LD   A, H
    OR   L
    JR   Z, ERROR_READ                      ; Archivo de 0 bytes

READ_OK:
    LD   A, (FILE_HANDLE)
    LD   B, A
    LD   C, _CLOSE
    CALL DOS2
    OR   A                                  ; Carry = 0 (Éxito)
    RET

ERROR_OPEN:
    LD   DE, ERR_OPEN_MSG                   ; Antes pasabas DRIVER_FILE_NAME sin '$' garantizado
    LD   B, 0D7h
    SCF
    RET                                     ; Vuelve a DRIVER_LOAD -> JP C, FATAL_ERROR

ERROR_READ:
    LD   A, (FILE_HANDLE)
    LD   B, A
    LD   C, _CLOSE
    CALL DOS2
    LD   DE, ERR_READ_MSG
    LD   B, 0FAh
    SCF
    RET                                     ; Vuelve a DRIVER_LOAD -> JP C, FATAL_ERROR
REPORT_AND_EXIT:
    PUSH DE
    PUSH BC
    LD   C, 09h          
    CALL DOS2    
    POP  BC       
    POP  DE
    LD   C, 62h             ; _TERM (Terminate with return code)
    CALL DOS2              ; ¡Adiós! El DOS reajusta todo y sale al prompt.   
