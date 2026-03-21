    PUBLIC MAPPER_JUMP_TABLE
    PUBLIC MAIN_SEGMENT
    EXTERN INIT_MAPPER_POINTERS
    EXTERN GET_FREE_SEGMENT
    EXTERN SELECT_SEGMENT_P2
    EXTERN GET_CURRENT_SEGMENT_P2

; --- CONSTANTS ---
P02             EQU 8000h
MAXBYTES        EQU 4000h 
EXTBIOS_HOOK    EQU 0FFCAh  ; System Hook for Extended BIOS (ADDRESS) 

; --- MSX-DOS 2 FUNCTIONS ---
_OPEN           EQU 43h
_READ           EQU 48h
_CLOSE          EQU 45h
_TERM           EQU 00h
DOS2            EQU 0005h    

; --- DRIVER HEADER OFFSETS (Matches your relocatable driver) ---
DRV_MAPPER_PTR  EQU 2       ; MAPPER_JUMP_TABLE (DEFW)
DRV_MAIN_SEG    EQU 4       ; MAIN_SEGMENT (DEFB)
DRV_DEV_SEG     EQU 5       ; DEVICE_RAM_SEGMENT (DEFB)
DRV_OLD_EXT     EQU 6       ; OLD_EXTBIOS_STUB (5 BYTES)
DRV_MY_ADDR     EQU 11       ; MY_ADDRESS (DEFW)
DRV_SIZE        EQU 13      ; DRV_SIZE (DEFW)
PATCH_FUNCTION  EQU 17      ; PATCH FUNCTION

    ORG 100H
    JP DRIVER_LOAD

; --- MAPPER SUPPORT DATA ---
MAPPER_JUMP_TABLE:  DEFW 0000h
MAIN_SEGMENT:       DEFB 00h
DEVICE_RAM_SEGMENT: DEFB 00h

; --- DATA STORAGE ---
DRIVER_FILE_NAME:   DEFS 64, 0   ; Buffer for ASCIIZ filename
                    DEFM  13, 10, "$"
FILE_HANDLE:        DEFB 0

; --- MESSAGES ---
ERR_USAGE_MSG:      DEFM "Usage: LDEXT.COM <filename.bin>", 13, 10, "$"
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
    
    ; Request a free 16KB segment
    CALL GET_FREE_SEGMENT
    LD   DE, ERR_MAPPER_MSG
    JP   C, FATAL_ERROR
    LD   (DEVICE_RAM_SEGMENT), A 

    ; Map the new segment to Page 2 (8000h)
    CALL SELECT_SEGMENT_P2  
    
    ; Load file from disk to 8000h
    CALL LOAD0       
    JP   C, FATAL_ERROR  

    ; --- SETUP DRIVER HEADER ---
    LD   IX, P02                            ; IX points to loaded driver
    
    LD   HL, (EXTBIOS_HOOK)                 ; Read current hook code (5 bytes)
    LD   (IX + DRV_OLD_EXT), HL             ; Store it for chaining
    LD   HL, (EXTBIOS_HOOK+2)               ; Read current hook address
    LD   (IX + DRV_OLD_EXT+2), HL           ; Store it for chaining
    LD   A, (EXTBIOS_HOOK +5)
    LD   (IX + DRV_OLD_EXT+5), A                 


    LD   HL, (MAPPER_JUMP_TABLE)  
    LD   (IX + DRV_MAPPER_PTR), HL
    
    LD   A, (MAIN_SEGMENT)  
    LD   (IX + DRV_MAIN_SEG), A
    
    LD   A, (DEVICE_RAM_SEGMENT)  
    LD   (IX + DRV_DEV_SEG), A

    ; --- CALCULATE DESTINATION (TOP OF STACK) ---
    DI
    LD   HL, SP                             ; Use current Stack Pointer as ceiling
    LD   DE, (IX + DRV_SIZE)                ; Get driver size from header
    LD   BC, DE                             ; BC = Size for LDIR
    
    OR   A                                  ; <--- ¡NUEVO! Limpia el Carry Flag
    SBC  HL, DE                             ; Ahora HL = HL - DE (sin errores)
  
    LD   (EXTBIOS_HOOK+1), HL                 ; Update system hook to point to driver
    LD   (IX + DRV_MY_ADDR), HL             ; Tell driver its own new address
    ; --- RELOCATE DRIVER ---
        
    CALL P02 + PATCH_FUNCTION
    EX   DE, HL                             ; DE = New destination (RAM High)
    LD   A, 0C3h                            ; HL = Source (8000h)
    LD   (EXTBIOS_HOOK), A
    LD   HL, P02                            ; HL = Source (8000h)
    LDIR                                    ; Copy driver to its permanent home

    ; --- RESTORE STATE AND CLEANUP ---
    POP  AF                                 ; Recuperamos el segmento original (estaba en la pila vieja)
    POP  DE                                 ; Recuperamos la dirección de retorno a DOS/BASIC en DE
    
    ; Ahora movemos la pila para proteger el driver
    LD   HL, (IX + DRV_MY_ADDR)             ; HL = Dirección base donde acabamos de copiar el driver
    LD   SP, HL                             ; ¡Instrucción válida! SP ahora apunta justo debajo del driver
    PUSH DE                                 ; Metemos la dirección de retorno en la NUEVA pila
    CALL SELECT_SEGMENT_P2                  ; Restauramos el segmento original en Página 2
    EI
    
    LD   DE, OK_MSG
    LD   C, 09h
    CALL DOS2
    LD   B, 0               ; Código de error 0 (Todo OK)
    LD   C, 62h             ; _TERM (Terminate with return code)
    CALL DOS2              ; ¡Adiós! El DOS reajusta todo y sale al prompt.     
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
