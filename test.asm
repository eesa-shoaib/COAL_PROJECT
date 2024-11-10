[org 0x100]
jmp start

att: dw 0x5520

start:
    ; Initialize video segment
    mov ax, 0xb800
    mov es, ax

    ; Clear the screen
    mov ax, 0x20
    xor di, di
    mov cx, 2000
    cld
    rep stosw

	mov ax, 0x5520
    xor di, di
    mov cx, 80
    cld
    rep stosw
	
	mov cx, 25
	xor di, di
	mov si, 158
loop1:	
	mov word[es: di], 0x5520
	mov word[es: si], 0x5520
	add di, 160
	add si, 160
	
	loop loop1
	
	mov ax, 0x5520
    sub di, 160
    mov cx, 80
    cld
    rep stosw
	
	
    ; Initialize character position at the top-left corner
    mov di, 162
    mov ax, 0x7720
    mov word[es:di], ax

mainLoop:
    ; Check if a key is pressed
    mov ah, 08h            
    int 21h                

    cmp al, 'w'            
    jz moveUp        
    
	cmp al, 'a'            
    jz moveLeft 
	
	cmp al, 's'            
    jz moveDown 
	
	cmp al, 'd'            
    jz moveRight 
	
	jmp mainLoop

moveUp: 
	
	mov si, di
	sub si, 160 
	mov ax, [es:si]
	cmp ax, [att]
	jz l1
	
	mov word[es: di], 0x20
	sub di, 160
	mov word[es:di], 0x7720
l1:
	jmp mainLoop

moveDown: 

	mov si, di
	add si, 160 
	mov ax, [es:si]
	cmp ax, [att]
	jz l2
	
	mov word[es: di], 0x20
	add di, 160
	mov word[es:di], 0x7720
l2:
	jmp mainLoop

moveLeft:
	
	mov si, di
	sub si, 2 
	mov ax, [es:si]
	cmp ax, [att]
	jz l3
	
	mov word[es: di], 0x20
	sub di, 2
	mov word[es:di], 0x7720
l3:	
	jmp mainLoop

moveRight:
	
	mov si, di
	add si, 2 
	mov ax, [es:si]
	cmp ax, [att]
	jz l4
	
	mov word[es: di], 0x20
	add di, 2
	mov word[es:di], 0x7720
l4:
	
	jmp mainLoop


exitProgram:
    mov ax, 4C00h
    int 21h
