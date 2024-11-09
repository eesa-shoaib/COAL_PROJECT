[org 0x100]
jmp start

playerAttribute: dw 0x70F0,0x70AF
enemyAttribute: dw 0x6745, 0x674E 
wallAttribute: dw 0x10AE
trackAttribute: dw 0x7720
perkAttribute: dw 0x7507


maze1: db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
      db 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1
      db 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1
      db 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1
      db 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1
      db 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1
      db 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1
      db 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 4, 0, 0, 1, 0, 1
      db 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1
      db 1, 0, 0, 0, 0, 0, 4, 1, 0, 0, 0, 1, 0, 0, 0, 1
      db 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1
      db 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 1, 1, 1
      db 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1
      db 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1
	  db 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1
      db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1

maze2: db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
      db 2, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1
      db 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1
      db 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1
      db 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1
      db 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1
      db 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1
      db 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1
      db 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1
      db 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 4, 0, 0, 1
      db 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1
      db 1, 0, 0, 4, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 1
      db 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1
	  db 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1
      db 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1
      db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1

maze3: db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
       db 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1
       db 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1
       db 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1
       db 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1
       db 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 4, 0, 1, 0, 0, 1
       db 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1
       db 1, 0, 0, 1, 0, 0, 0, 0, 3, 0, 0, 1, 0, 1, 0, 1
       db 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1
       db 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1
       db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1
       db 1, 0, 0, 0, 1, 0, 0, 0, 0, 4, 0, 1, 0, 0, 0, 1
       db 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1
       db 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1
       db 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1
       db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1

randNum: dw 0

clrsrc:
		
	mov ax, 0xb800
	mov es, ax
	mov ax, 0x20
	xor di, di
	mov cx, 2000
	cld
	rep stosw
	
	ret

GenRandNum:
    push bp
    mov bp, sp
    push cx
    push ax
    push dx

    mov ah, 00h         ; Interrupt to get system time
    int 1Ah             ; CX:DX now hold number of clock ticks since midnight
    mov ax, dx          
    xor dx, dx          
    mov cx, 4           ; Set divisor to 4 to get a remainder between 0 and 3
    div cx              

    mov [randNum], dx   

    pop dx
    pop ax
    pop cx
	pop bp
	ret
	
delay:
	push cx
	mov cx, 1  ; change the values to increase delay time

delay_loop1:
	push cx
	mov cx, 0xFFFF

delay_loop2:

	loop delay_loop2
	pop cx
	loop delay_loop1

	pop cx
	ret	

LoadMaze:
	push bp
	mov bp, sp
	pusha
	
	mov ax, 0xb800
	mov es, ax
	
	mov cx, 256			; as the dimension of maze is 16*16
	mov si, [bp + 4]
	mov di, 1328		; row = 8, col = 24
	xor dx, dx
	
MazeGenerationLoop:
	
	cmp dx, 64 
	jnz drawing
	
	sub di, 64
	add di, 160
	mov dx, 0
	
drawing:	

	cmp byte[si], 4
	jz perk
	
	cmp byte[si], 3
	jz enemy
	
	cmp byte[si], 2
	jz player
	
	cmp byte[si], 0
	jz Track
	
	
	; wall printing
	mov ax, [wallAttribute]
	mov word [es:di],ax
	add di, 2
	mov word [es:di],ax
	add di, 2
	add dx, 4
	xor ax, ax				; setting bx back to 0

	inc si
	call delay
	
	loop MazeGenerationLoop
	jmp terminate

perk:
	call printPerk
	add dx, 4
	inc si
	call delay
	
	loop MazeGenerationLoop
	jmp terminate
	

player:
	call printPlayer
	add dx, 4
	inc si
	call delay
	
	loop MazeGenerationLoop
	jmp terminate
	

enemy:
	call printEnemy
	add dx, 4
	inc si
	call delay
	
	loop MazeGenerationLoop
	jmp terminate
	
Track:
	call printTrack
	add dx, 4
	inc si
	call delay
	
	loop MazeGenerationLoop

terminate:	
	
	sub di, 8
	mov word[es:di], 0xFCDC
	add di, 2
	mov word[es:di], 0xFCDC
	
	popa	
	pop bp
	ret 2

printPerk:
	mov ax, [perkAttribute]
	mov word [es:di],ax
	add di, 2
	mov word [es:di],ax
	add di, 2
	xor ax, ax				; setting ax back to 0

	ret

printPlayer:
	
	mov ax, [playerAttribute]
	mov word [es:di],ax
	add di, 2
	mov ax, [playerAttribute + 2]
	mov word [es:di],ax
	add di, 2
	xor ax, ax				; setting ax back to 0

	ret

printEnemy:
	mov ax, [enemyAttribute]
	mov word [es:di],ax
	add di, 2
	mov ax, [enemyAttribute + 2]
	mov word [es:di], ax
	add di, 2
	xor ax, ax				; setting ax back to 0

	ret

printTrack:

	mov ax, [trackAttribute]
	mov word [es:di],ax
	add di, 2
	mov word [es:di],ax
	add di, 2
	xor ax, ax				; setting ax back to 0

	ret
	
start:

	
	call clrsrc
	
	call GenRandNum
	mov ax, [randNum]
	
	mov al, [randNum]
    cmp al, 0            ; If randNum == 0, load maze1
    je LoadMaze1         

    cmp al, 1            ; If randNum == 1, load maze2
    je LoadMaze2         

    cmp al, 2            ; If randNum == 2, load maze3
    je LoadMaze3         

LoadMaze1:
    push maze1          
    call LoadMaze       
    jmp EndProgram      

LoadMaze2:
    push maze2          
    call LoadMaze       
    jmp EndProgram       
	
LoadMaze3:
    push maze3           
    call LoadMaze        
    jmp EndProgram       

EndProgram:

    mov ax, 0x4c00       
    int 0x21