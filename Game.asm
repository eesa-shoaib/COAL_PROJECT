[org 0x100]
jmp start

playerAttribute: dw 0x38F0,0x38AF
enemyAttribute: dw 0x6745, 0x674E 
wallAttribute: dw 0x10AE
trackAttribute: dw 0x7720
perkAttribute: dw 0x7507
HearthAttribute:  dw 0x0403
finishAttribute: dw 0xFCDC

score: dw 0
healthCount: dw 3

finishIndex: dw 0
playerIndexDisplay: dw 0

maze1: db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
      db 1, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1
      db 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1
      db 1, 0, 0, 0, 0, 0, 3, 1, 0, 0, 0, 0, 0, 1, 0, 1
      db 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1
      db 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1
      db 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1
      db 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 4, 0, 0, 1, 0, 1
      db 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1
      db 1, 0, 0, 0, 0, 3, 4, 1, 0, 0, 0, 1, 0, 0, 0, 1
      db 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1
      db 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 1, 1, 1
      db 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1
      db 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1
	  db 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1
      db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1

maze2: db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
      db 1, 2, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1
      db 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1
      db 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1
      db 1, 0, 1, 1, 1, 1, 3, 1, 0, 1, 1, 1, 0, 1, 1, 1
      db 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1
      db 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1
      db 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1
      db 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1
      db 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 4, 0, 0, 1
      db 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1
      db 1, 0, 0, 4, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 1
      db 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1
	  db 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1
      db 1, 0, 0, 0, 1, 0, 3, 0, 0, 0, 0, 0, 0, 1, 0, 1
      db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1

maze3: db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
       db 1, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1
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
       db 1, 0, 0, 0, 3, 0, 1, 3, 0, 0, 0, 0, 0, 1, 0, 1
       db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1

maze4: db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
       db 1, 2, 0, 0, 0, 0, 3, 0, 0, 1, 0, 0, 0, 0, 0, 1
       db 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1
       db 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1
       db 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1
       db 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 4, 0, 1, 0, 0, 1
       db 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1
       db 1, 0, 0, 1, 0, 0, 0, 0, 3, 0, 0, 1, 0, 1, 0, 1
       db 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1
       db 1, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 1, 0, 0, 0, 1
       db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1
       db 1, 0, 0, 0, 1, 0, 0, 0, 0, 4, 0, 1, 0, 0, 0, 1
       db 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1
       db 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1
       db 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1
       db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1



randNum: dw 0
tickcount:    dw   20                  ; start from 60 seconds
tickticks:    dw   0                   ; counter for 18 ticks (1 second)

startTimer:
	xor  ax, ax 
	mov  es, ax                
	cli                         
	mov  word [es:8*4], timer   
	mov  [es:8*4+2], cs         
	sti                        
	
	
	
ret

;-----{Printing Number}-----

printnum:     push bp 
              mov  bp, sp 
              push es 
              push ax 
              push bx 
              push cx 
              push dx 
              push di 
 
              mov  ax, 0xb800 
              mov  es, ax             
              mov  ax, [bp+4]         
              mov  bx, 10             
              mov  cx, 0 
		
 
nextdigit:    mov  dx, 0              
              div  bx                 
              add  dl, 0x30           
              push dx                 
              inc  cx                 
              cmp  ax, 0               
              jnz  nextdigit          
 
              mov  di, 140           
				
				cmp cx, 1
				jz singledigit
				
nextpos:      pop  dx                
              mov  dh, 0x07           
              mov  [es:di], dx         
              add  di, 2              
              loop nextpos            
				jmp term
				
singledigit:				
			  pop  dx                
              mov  dh, 0x84           
              mov  [es:di], dx         
              add  di, 2              
              loop singledigit
			  
			  mov  word[es:di], 0x0720
			  
term: 
              pop  di 
              pop  dx 
              pop  cx 
              pop  bx 
              pop  ax
              pop  es 
              pop  bp 
              ret  2 

gameOver:    
			  call clrsrc
			  ret

timer:      push ax 
            push bx
 
            inc  word [cs:tickticks]    
 

            mov  ax, [cs:tickticks]
            cmp  ax, 18               ; Check if tick counter reached 18 (approximately 1 second)
            jb   skip_display_update    

            mov  word [cs:tickticks], 0 
		
            dec  word [cs:tickcount]    

            mov  ax, [cs:tickcount]
            cmp  ax, 0
            jg   display_update         
              
            call gameOver                  

	skip_display_update:
              
            jmp end_interrupt

	display_update:
              
            push word [cs:tickcount]
            call printnum

	end_interrupt:
            mov  al, 0x20 
            out  0x20, al              
 
            pop  bx 
            pop  ax 
            iret 
			  
clrsrc:
		
	mov ax, 0xb800
	mov es, ax
	mov ax, 0x20
	xor di, di
	mov cx, 2000
	cld
	rep stosw
	
	ret

;-----{Generation of Random Number}-----

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


;-----{Maze Generation}-----

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
		call printWall
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
		sub di, 160
		mov [finishIndex], di
		mov ax, [finishAttribute]
		mov word[es:di], ax
		add di, 2
		mov word[es:di], ax
		
		popa	
		pop bp
		ret 2


;-----{Loading Textures on Display Memory}-----

printWall: 
	mov ax, [wallAttribute]
	mov word [es:di],ax
	add di, 2
	mov word [es:di],ax
	add di, 2
	xor ax, ax
	
	ret
	
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
	mov [playerIndexDisplay], di
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
	
	
;-----{Movement for Player}-----

playerMovement:
	
	mov di, [playerIndexDisplay]
	mainLoop:
		
		; if health is zero end the game
		xor bx, bx
		cmp word[cs:healthCount], bx
		jz exitProgram
		
		; if player has moved to finish end the game
		cmp di, [finishIndex]
		jz exitProgram
		
    ; Check if a key is pressed
		mov ah, 00h            
		int 16h  
		
		cmp al, 27				; escape
		jz exitProgram
	
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
		cmp ax, [wallAttribute]
		jz l1
		
		call ScoreHealthUpdate
		
		push di
		call printTrack
		pop di
		
		sub di, 160
			
		; character printing
		mov dx, [playerAttribute]
		mov word[es: di], dx
		mov dx, [playerAttribute + 2]
		mov word[es: di + 2], dx
		
		l1:
			jmp mainLoop

	moveDown: 

		mov si, di
		add si, 160 
		mov ax, [es:si]
		cmp ax, [wallAttribute]
		jz l2
		
		call ScoreHealthUpdate
		
		push di
		call printTrack
		pop di
		
		add di, 160
			
		; character printing
		mov dx, [playerAttribute]
		mov word[es: di], dx
		mov dx, [playerAttribute + 2]
		mov word[es: di + 2], dx
		
		l2:
			jmp mainLoop

	moveLeft:
	
		mov si, di
		sub si, 4 
		
		mov ax, [es:si]
		cmp ax, [wallAttribute]
		jz l3
		
		call ScoreHealthUpdate
		
		push di
		call printTrack
		pop di
		
		sub di, 4
			
		; character printing
		mov dx, [playerAttribute]
		mov word[es: di], dx
		mov dx, [playerAttribute + 2]
		mov word[es: di + 2], dx

		l3:	
			jmp mainLoop

	moveRight:
	
		mov si, di
		add si, 4 
		mov ax, [es:si]
		cmp ax, [wallAttribute]
		jz l4
		
		call ScoreHealthUpdate
		
		push di
		call printTrack
		pop di
		
		add di, 4
			
		; character printing
		mov dx, [playerAttribute]
		mov word[es: di], dx
		mov dx, [playerAttribute + 2]
		mov word[es: di + 2], dx

		l4:
			jmp mainLoop

	exitProgram:
			
			call delay
			call delay
			call delay
			call delay
			call delay
			call delay
			
			call clrsrc
			ret
;----{Score and Health Update}----
ScoreHealthUpdate:

; es is already set to 0xB800
; ax holds the attribute of position to move before  entering this subroutine	
	cmp ax, [perkAttribute]
	jnz skipScoreUpdate
	
	; add ten in score when perk collected
	mov bx, [score]
	add bx, 10
	mov [score], bx
		
	call displayScore
	
	skipScoreUpdate:		
		cmp ax, [enemyAttribute]
		jnz skipHealthUpdate
		
		; decrement on clash with the enemy
		dec word [cs:healthCount]
		call displayHealth
	
	skipHealthUpdate:
		
		call displayScore
		
		ret

displayHealth:
    pusha                         
    mov cx, [healthCount]         
    mov ax, [HearthAttribute]     
    mov di, 0					                
    
    mov bx, 3                    
	
	;clear the previous hearts
	clear_hearts:
		mov word [es:di], 0x20      
		add di, 2
		dec bx
		jnz clear_hearts

		mov di, 0x0
		  
		cmp cx, 0
		jnz display_hearts                       
		
		; if zero hearts left no need to display heart
		jmp _end
		
	display_hearts:
		mov [es:di], ax               
		add di, 2                     
		loop display_hearts           

	_end:
		popa   
		
    ret


displayScore:
	pusha 
	
	mov ax, [score]
	mov  bx, 10             
    mov  cx, 0               
	
	;storing the digits to display
	_nextdigit:
			mov  dx, 0              
            div  bx                 
            add  dl, 0x30           
            push dx                 
            inc  cx                 
            cmp  ax, 0               
            jnz  _nextdigit          
 
            mov  di, 162           
	
	; displaying it :)
	_nextpos:    
			pop  dx                
            mov  dh, 0x07           
            mov  [es:di], dx         
            add  di, 2              
            loop _nextpos            
 
	popa
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
	
	cmp al, 3            ; If randNum == 2, load maze3
    je LoadMaze4  
	
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

LoadMaze4:
	push maze4
	call LoadMaze
		
EndProgram:

	call displayHealth
	call displayScore
	
	call playerMovement
	
    mov ax, 0x4c00       
    int 0x21