    PUBLIC MAPPER_JUMP_TABLE
    PUBLIC MAIN_SEGMENT
    EXTERN INIT_MAPPER_POINTERS
    EXTERN ALLOCATE_SEGMENT
    EXTERN SELECT_SEGMENT_P2
    EXTERN GET_CURRENT_SEGMENT_P2

; --- CONSTANTS ---
P02             EQU 8000h
MAXBYTES        EQU 4000h 
TRAMPOLINE_DEST EQU  0F41FH  ; KBUF (F41FH, 318)
                            ; contents:	buffer to store characters typed; where direct statements
		                    ; are stored in ASCII code...we use to keep the trampolines
NUMBER_OF_ALLOCATED_DRV EQU  0F55Ch ;last byte used in Basic we use it to stone the number of trampolines.
EXTBIOS_HOOK    EQU  0FFCAh  ; System Hook for Extended BIOS (ADDRESS) 

; --- MSX-DOS 2 FUNCTIONS ---
_OPEN           EQU 43h
_READ           EQU 48h
_CLOSE          EQU 45h
_TERM           EQU 00h
DOS2            EQU 0005h    


; --- DRIVER HEADER OFFSETS  ---
DEVICE_ID       EQU 8002h      ; DEVICE ID 

CALLS           EQU 0Fh      ; Interslot call

    ORG 100H
    JP DRIVER_LOAD

;------------------------TRAMPOLINE DEFINITION---------------------------------
    ; D is device ID
    ; E is function ID
TRAMPOLINE: 
    JR START_TRAMPOLINE
    OLD_STUB_CODE:       DEFM 0C9h,0C9h,0C9h,0C9h,0C9h          ; +6 (Old STUB)
START_TRAMPOLINE:
    PUSH IX
    PUSH IY
    PUSH DE
    PUSH AF
    LD   A, D
_PATCH_DEVICEID:
    CP   00h
    JR   NZ, NEXT_DEVICE  
    POP AF
    POP DE       ;D is deviceid and E = function
    POP IY
_PATCH_MAPPERTABLE:
    CALL 0000h 
  
_PATCH_SEGMENT:
    DB   03h       ; Tu número de segmento PATCH_SEGMENT
    DW   P02       ; La dirección de destino
    POP  IX
    RET
NEXT_DEVICE:
    POP  AF                 ; Restore AF from the very beginning
    POP  DE
    POP  IY
    POP  IX
    JR OLD_STUB_CODE
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
    
    ; Initialize Mapper and Check Support
    XOR  A
    CALL INIT_MAPPER_POINTERS 
    LD   DE, ERR_BIOS_MSG
    JP   C, REPORT_AND_EXIT
    
    ; Get current segment to restore it later
    CALL GET_CURRENT_SEGMENT_P2
    PUSH AF                 
    LD    A,1    ; Request a free 16KB SYSTEM segment
    CALL ALLOCATE_SEGMENT
    LD   DE, ERR_MAPPER_MSG
    JP   C, FATAL_ERROR
    LD   (DEVICE_RAM_SEGMENT), A 

    ; Map the new segment to Page 2 (8000h)
    CALL SELECT_SEGMENT_P2  
    
    ; Load file from disk to 8000h
    CALL LOAD0       
    JP   C, FATAL_ERROR  

    ; --- SETUP DRIVER HEADER ---
    LD   IX, OLD_STUB_CODE                  ; IX points to trampoline section
    LD   HL, (EXTBIOS_HOOK)                 ; Read current hook code (5 bytes)
    LD   (IX), HL                           ; Store it for chaining
    LD   HL, (EXTBIOS_HOOK + 2)               ; Read current hook address
    LD   (IX + 2), HL                         ; Store it for chaining
    LD   A, (EXTBIOS_HOOK + 4)               ; and the 5th byte (starting by 0)
    LD   (IX + 4), A                 

    ; PATCH TRAMPOLINE
    LD   IX, _PATCH_MAPPERTABLE             ; IX points to trampoline section
    LD   HL, (MAPPER_JUMP_TABLE)           
    ADD  HL, CALLS
    LD   (IX + 1), HL                       ; +1 opcode
    LD   A, (DEVICE_ID)  
    LD   IX , _PATCH_DEVICEID
    LD   (IX + 1), A   
    LD   A, (DEVICE_RAM_SEGMENT)  
    LD   IX , _PATCH_SEGMENT
    LD   (IX), A  

    ; --- CALCULATE DESTINATION (TOP OF STACK) ---
    ; TODO-CALCULATE THE DESTINATION DEPENDING ON THE NUMBER OF INSTALLED TRAMPOLINES
    ; store in NUMBER_OF_ALLOCATED_DRV
    LD   BC, END_OF_TRAMPOLINE - TRAMPOLINE                ; Get driver size from header
                                                           ; BC = Size for LDIR
    LD   DE, TRAMPOLINE_DEST                ; Ahora HL = HL - DE 
    LD   HL, (NUMBER_OF_ALLOCATED_DRV)
 _NEXT_DRIVER:
    INC  L
    DEC  L
    JR   Z,_CONTINUE
    LD   A , C 
    ADD  DE, A
    DEC  L
    JR   NZ,_NEXT_DRIVER
_CONTINUE:
    OR   A                                  ; <--- ¡NUEVO! Limpia el Carry Flag
    DI
    LD   (EXTBIOS_HOOK+1), DE               ; Update system hook to point to trampoline address
    ; --- RELOCATE DRIVER ---
    LD   A, 0C3h                            ; jp DE = dest (trampoline))
    LD   (EXTBIOS_HOOK), A
    LD   HL, TRAMPOLINE                     ; HL = Source (trampoline)
    LD   BC, (NUMBER_OF_ALLOCATED_DRV)
    INC  BC 
    LD   (NUMBER_OF_ALLOCATED_DRV),BC
    LD   BC, END_OF_TRAMPOLINE - TRAMPOLINE ; BC has the size
    LDIR                                    ; Copy driver to its permanent home
    ; --- RESTORE STATE AND CLEANUP ---
    POP  AF                                 ; Recuperamos el segmento original (estaba en la pila vieja)
    CALL SELECT_SEGMENT_P2                  ; Restauramos el segmento original en Página 2
    EI
    LD   DE, OK_MSG
    LD   C, 09h
    CALL DOS2
    LD   B, 0               ; Código de error 0 (Todo OK)
    LD   C, 62h             ; _TERM (Terminate with return code)
    CALL DOS2               ; ¡Adiós! El DOS reajusta todo y sale al prompt.     
FATAL_ERROR:
    POP  AF                                 ; Clean stack (A = Original Segment)
    CALL SELECT_SEGMENT_P2                  ; <--- ¡NUEVO! Restaurar P2 antes de salir
    LD   B, 0DFh              ; Código de error DF internal error
    JR   REPORT_AND_EXIT

; --- DISK LOADING ROUTINE ---
LOAD0:  
    ; 1. Open File
    LD   DE, DRIVER_FILE_NAME
    LD   A, 0                               ; Read-only mode
    LD   C, _OPEN
    CALL DOS2
    OR   A               
    JR   NZ, ERROR_OPEN
    LD   A,B
    LD   (FILE_HANDLE), A

    ; 2. Read to 8000h
    LD   B, A                               ; B = Handle
    LD   DE, P02                            ; Destination
    LD   HL, MAXBYTES                       ; Max 16KB
    LD   C, _READ
    CALL DOS2
    OR   A
    JR   NZ, ERROR_READ

    ; 3. Close File
    LD   A, (FILE_HANDLE)
    LD   B, A
    LD   C, _CLOSE
    CALL DOS2
    RET

; --- ERROR HANDLERS ---
ERROR_OPEN:
    LD   DE, DRIVER_FILE_NAME
    LD   B, 0D7h              ; Código de error d7 file not found
    SCF                  
    JR REPORT_AND_EXIT                  

ERROR_READ:
    PUSH AF              
    LD   A, (FILE_HANDLE)
    LD   B, A
    LD   C, _CLOSE
    CALL DOS2            
    POP  AF
    LD   DE, DRIVER_FILE_NAME
    LD   B, 0FAh              ; Código de error FA data error)
    SCF                  

REPORT_AND_EXIT:
    PUSH DE
    PUSH BC
    LD   C, 09h          
    CALL DOS2    
    POP  BC       
    POP  DE
    LD   C, 62h             ; _TERM (Terminate with return code)
    CALL DOS2              ; ¡Adiós! El DOS reajusta todo y sale al prompt.   
