; working score and health calculation
; movement works
; better load maze
[org 0x100]
jmp start

playerAttribute: dw 0x38F0,0x38AF
enemyAttribute: dw 0x6745, 0x674E 
wallAttribute: dw 0x10AE, 0x10AE
trackAttribute: dw 0x7720, 0x7720
perkAttribute: dw 0x7507, 0x7507
HearthAttribute:  dw 0x0403, 0x0403
finishAttribute: dw 0xFCDC, 0xFCDC

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

;-----{Printing Number}-----
printnum:     
	push bp 
    mov  bp, sp 
    pusha
	
    mov  ax, 0xb800 
    mov  es, ax             
    mov  ax, [bp+4]         
    mov  bx, 10             
    mov  cx, 0 
		
 
	nextdigit: 
		mov  dx, 0              
        div  bx                 
        add  dl, 0x30           
        push dx                 
        inc  cx                 
        cmp  ax, 0               
    jnz  nextdigit          
 
    mov  di, 140           

	cmp cx, 1
	jz singledigit
	
	nextpos:      
		pop  dx                
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
		popa 
		pop bp 

	ret  2 

gameOver:    
	call clearMaze
	ret

timer:      
	push ax 
    push bx
 
    inc  word [cs:tickticks]    
	
    mov  ax, [cs:tickticks]
    cmp  ax, 0x3A               ; Check if tick counter reached 18 (approximately 1 second)
    jb   skip_display_update    

    mov  word [cs:tickticks], 0 
    dec  word [cs:tickcount]    

    mov  ax, [cs:tickcount]
    cmp  ax, 0
    jg   display_update         
	
	skip_display_update:
        jmp end_

	display_update:
        push word [cs:tickcount]
        call printnum

	end_:
        pop bx 
        pop ax 
    ret
  
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
    pusha

    mov ah, 00h         ; Interrupt to get system time
    int 1Ah             ; CX:DX now hold number of clock ticks since midnight
    mov ax, dx          
    xor dx, dx          
    mov cx, 4           ; Set divisor to 4 to get a remainder between 0 and 3
    div cx              

    mov [randNum], dx   

    popa
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
		; Check if we've reached the end of the row
			cmp dx, 16
			jne drawing
	
			; Move to the next row
			sub di, 64         ; Adjust for new row
			add di, 160        ; Move down one row in video memory
			xor dx, dx         ; Reset column counter
			
		drawing:
			; Load the current maze element
			mov al, [si]
			inc si             ; Move to the next maze cell for the next iteration
		
			; Determine the drawing attribute
			cmp al, 1          ; Wall
			je draw_wall
			cmp al, 2          ; Player
			je draw_player
			cmp al, 3          ; Enemy
			je draw_enemy
			cmp al, 4          ; Perk
			je draw_perk
			
			jmp empty_space    ; Track
		
		draw_wall:
			mov bx, wallAttribute
			jmp draw_cell
		
		draw_player:
			mov [playerIndexDisplay], di
			mov bx, playerAttribute
			jmp draw_cell
		
		draw_enemy:
			mov bx, enemyAttribute
			jmp draw_cell
		
		draw_perk:
			mov bx, perkAttribute
			jmp draw_cell
		
		empty_space:
			mov bx, trackAttribute
		
		draw_cell:
			; Draw the current cell to video memory
			mov ax, [bx]
			mov [es:di], ax
			add di, 2         ; Move to the next screen position
			mov ax, [bx + 2]
			mov [es:di], ax
			add di, 2         ; Move to the next screen position
			inc dx            ; Increment column counter
			
			call delay
			
	loop MazeGenerationLoop

	terminate:	
		
		sub di, 8
		mov [finishIndex], di
		mov ax, [finishAttribute]
		mov word[es:di], ax
		add di, 2
		mov word[es:di], ax
		
		popa	
		pop bp
	ret 2


;-----{Loading Textures on Display Memory}-----

printTrack:

	mov ax, [trackAttribute]
	mov word [es:di],ax
	mov word [es:di + 2],ax
	xor ax, ax				; setting ax back to 0

ret

printPlayer:
	mov dx, [playerAttribute]
	mov word[es: di], dx
	mov dx, [playerAttribute + 2]
	mov word[es: di + 2], dx
	
ret
	
;-----{Movement for Player}-----

playerMovement:
	
	mov di, [playerIndexDisplay]
	
	mainLoop:
		
		mov ah, 01
		int 0x16
		jnz movementloop
		
		call delay
		call timer
		
		mov ax, [tickcount]
		cmp ax, 1
		jz exitProgram
		
		jmp mainLoop
		movementloop:
			; if health is zero end the game
			xor bx, bx
			cmp word[cs:healthCount], bx
			jz exitProgram
			
			; if player has moved to finish end the game
			cmp di, [finishIndex]
			jz exitProgram
				
			mov ax, [tickcount]
			cmp ax, 0
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
				jz mainLoop
				
				call ScoreHealthUpdate
				call printTrack
				
				; drawing player moving
				sub di, 160
				call printPlayer
				
			jmp mainLoop

			moveDown: 

				mov si, di
				add si, 160 
				mov ax, [es:si]
				cmp ax, [wallAttribute]
				jz mainLoop
				
				call ScoreHealthUpdate
				call printTrack
				
				; draw player moving
				add di, 160
				call printPlayer
				
			jmp mainLoop

			moveLeft:
			
				mov si, di
				sub si, 4 
				
				mov ax, [es:si]
				cmp ax, [wallAttribute]
				jz mainLoop
				
				call ScoreHealthUpdate
				call printTrack
				
				; draw player moving
				sub di, 4
				call printPlayer

				
			jmp mainLoop

			moveRight:
			
				mov si, di
				add si, 4 
				mov ax, [es:si]
				cmp ax, [wallAttribute]
				jz mainLoop
				
				call ScoreHealthUpdate
				call printTrack
				
				; draw player moving
				add di, 4
				call printPlayer

			jmp mainLoop

	exitProgram:
			
		call clearMaze
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
    mov di, 2					                
    
    mov bx, 3                    
	
	;clear the previous hearts
	clear_hearts:
		mov word [es:di], 0x20      
		add di, 2
		dec bx
	jnz clear_hearts


	mov di, 2	
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
    jnz _nextdigit          
 
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
	
clearMaze:
		mov ax, 0xb800
		mov es, ax
		
		mov cx, 256			; as the dimension of maze is 16*16
		mov di, 1328		; row = 8, col = 24
		mov ax, 0x20
		xor dx, dx
		
		MazeClearingLoop:
		
			cmp dx, 64 
			jnz pixelClear
			
			sub di, 64
			add di, 160
			mov dx, 0
			
			pixelClear:	

				mov word [es:di], 0x20
				add di, 2
				mov word [es:di], 0x20
				add di, 2
				add dx, 4			; setting bx back to 0

				call delay	
		loop MazeClearingLoop
	call clrsrc	
	
	ret 