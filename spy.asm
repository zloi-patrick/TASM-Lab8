.286
.MODEL tiny
.STACK 100h

.DATA
temp_buffer db 14 dup ('$')
error_parsing db "Failed to parse command line args", '$'
create_file_error_message db "Creating file failed", '$'
open_file_error_message db "Opening file failed", '$'
close_file_error_message db "Closing file failed", '$'
file_open_success_message db "File was successfully opened", '$'

path db "./spy.txt", 11 dup (0)

.CODE
old_handler dd 0 ; 4 bytes

temp_string db "temp string", '$'
file_descriptor dw 0
symbol db "C", 0
number dw 0
write_file_error_message db "Writing into file failed", '$'
error_status_code_message db "Error status code ->", '$'
endl db  10, 13, '$'
space db " ", '$'

jmp start

init macro
  mov ax, @data
  mov ds, ax
endm

exit macro
  mov ax, 4c00h
  int 21h
endm


exit_resident macro
  ; ;get exit code
  ; mov ah, 4dh
  ; int 21h

  ; mov al, ah    ; exit code
  ; mov ah, 31h   ; leave resident (KEEP)

  ; mov cx, es                      ; program start segment address (in paragraphs)
  ; mov bx, empty_segment           ; program end segment address (in paragraphs)
  ; sub bx, cx                      ; BX contain minimum required memory size for programm
  ; mov dx, bx
  ; int 21h

  mov dx, offset cs:end_resident
  int 27h
endm

;first param is output buffer
;second param is number address (offset from `ds`)
itoa:
  push bp
  mov bp, sp
  push di
  push si
  push bx
  push ax
  push dx
  
  mov di, ss:[bp + 6] ;output
  mov si, ss:[bp + 4] ;number address (number stored as word) 
  mov ax, word ptr [si] ;actual number
  mov bx, 10
  
  ;check if number is negative
  cmp word ptr [si], 0h
  jge positive_true
  
  mov byte ptr [di], '-'
  inc di
  neg ax
  positive_true:
  mov si, di
  
  itoa_iterate:
    mov dx, 00h
    cmp ax, 10
    jl itoa_end
    
    div bx
    add dx, 48
    mov byte ptr [di], dl
    inc di
    
    jmp itoa_iterate
  
  itoa_end:
  add ax, 48
  mov byte ptr [di], al
  
  push si
  push di
  call reverse
  pop di
  pop si
  
  inc di
  mov byte ptr[di], '$'
  
  pop dx
  pop ax
  pop bx
  pop si
  pop di
  pop bp
ret

reverse:
    push bp
    mov bp, sp
    push si
    push di
    push ax
    push bx
    push dx
    
    mov si, ss:[bp + 6] ;start address
    mov bx, ss:[bp + 4] ;end address
    mov di, bx
    
    cmp si, bx
    je reverse_end
    
    mov ah, 00h
    reverse_iterate:
        lodsb ; from memory with [si] address to `al` register
        dec si
        mov dl, byte ptr [di]
        mov byte ptr ds:[si], dl
        stosb ;from `al` register to memory with [di] address 
        inc si
        sub di, 2
        
        cmp si, di
        je reverse_end
        
        dec si
        cmp si, di
        je reverse_end
        inc si
        jne reverse_iterate
    
    
    reverse_end:    
    
    pop dx
    pop bx
    pop ax
    pop di
    pop si    
    pop bp
ret

call_itoa macro
  push dx
  push offset temp_buffer
  push offset number
  call itoa
  pop dx
  pop dx
  pop dx
endm

log proc
  push bp
  mov bp, sp
  push ax
  mov ax, 0900h
  mov dx, ss:[bp+4]
  int 21h
  pop ax
  pop bp
  ret
endp

call_log macro value
  push offset value
  call log
  pop dx
endm

new_handler proc far
  ; init

  pushf
  call dword ptr cs:old_handler

  ; sti
  push ds
  push cs
  pop ds

  mov ax, 0900h
  mov dx, offset ds:temp_string
  int 21h

  ; ; pusha
  ; mov bx, word ptr cs:[file_descriptor]
  ; mov dx, offset ds:symbol
  ; mov cx, 1
  ; mov ah, 40h
  ; int 21h
  ; jc write_file_error
  ; cmp ax, cx
  ; jb write_file_error

  ; jmp write_file_success
  ; write_file_error:
  ;   ; if error occured
  ;   mov word ptr cs:[number], ax
  ;   call_log write_file_error_message
  ;   call_log endl
  ;   ; call_log error_status_code_message
  ;   ; call_log space
  ;   ; call_itoa
  ;   ; call_log temp_buffer
  ;   ; exit    

  ; write_file_success:
  ; mov bx, word ptr ds:[file_descriptor]
  ; ; popa

  pop ds
  ; popf

  iret
endp

; write_into_file proc
;   pusha
;   mov bx, word ptr ds:[file_descriptor]
;   mov dx, offset symbol
;   mov cx, 1
;   mov ah, 40h
;   int 21h
;   jc write_file_error
;   cmp ax, cx
;   jb write_file_error

;   jmp write_file_success
;   write_file_error:
;     ; if error occured
;     mov word ptr ds:[number], ax
;     call_log write_file_error_message
;     call_log endl
;     call_log error_status_code_message
;     call_log space
;     call_itoa
;     call_log temp_buffer
;     exit    

;   write_file_success:
;   mov bx, word ptr ds:[file_descriptor]
;   popa
;   ret
; endp

; for calculating size when keeping resident 
end_resident:

parse_command_line proc
  push di
  push bx
  push ax
  push cx
  ; `es` contain `PSP` segment address
  ; store file name in `file_name`

  xor bx, bx
  mov bl, byte ptr es:[80h] ; cli args length
  cmp bl, 0
  jbe bad_args
  jmp parse_command_line_continue
  
  bad_args:
  jmp parse_command_line_end

  parse_command_line_continue:
  mov di, 81h
  mov al, ' '
  rep scasb ; skip all spaces
  ; dec di
  dec di

  xor cx, cx
  mov cl, byte ptr es:[80h]
  dec cl

  mov bx, offset path
  copy_path:
    mov al, byte ptr es:[di]
    mov byte ptr ds:[bx], al
    inc di
    inc bx
  loop copy_path
  mov byte ptr ds:[bx], 0
  
  parse_command_line_end:
  pop cx
  pop ax
  pop bx
  pop di
  ret
endp

open_file proc
  pusha
  mov cx, 0
  mov ah, 3ch
  ; mov al, 1h
  mov dx, offset path
  int 21h
  jnc open_file_success
  
    ; if error occured
    mov word ptr cs:[number], ax
    call_log open_file_error_message
    call_log endl
    call_log error_status_code_message
    call_log space
    call_itoa
    call_log temp_buffer
    exit

  open_file_success:
  call_log file_open_success_message
  call_log endl
  mov word ptr cs:[file_descriptor], ax ; store file descriptor
  popa
  ret
endp

override proc
  cli
  pusha
  mov ah, 35h     ; get int vect
  mov al, 9h      ; IRQ 1 - key pressed and released
  int 21h
  
  ; store
  mov word ptr cs:[old_handler], bx     ; handler address
  mov word ptr cs:[old_handler + 2], es ; segment handler adress

  push ds
  push cs
  pop ds

  mov dx, offset ds:new_handler
  mov ah, 25h     ; get int vect
  mov al, 9h      ; IRQ 1 - key pressed and released
  int 21h
  
  pop ds
  popa
  sti

  ret
endp

start:
  init
  call parse_command_line

  ; override int 09h (store old & set new)
  call override
  
  ; open file
  call open_file

  ; write bullshit into file
  ; call write_into_file

  ; ; close file
  ; mov ah, 3eh
  ; int 21h
  ; jnc start_end

  ;   ; if error occured
  ;   mov word ptr ds:[number], ax
  ;   call_log close_file_error_message
  ;   call_log endl
  ;   call_log error_status_code_message
  ;   call_log space
  ;   call_itoa
  ;   call_log temp_buffer
  ;   exit    

  ; leave resident
  start_end:
  ; exit
  exit_resident
end start