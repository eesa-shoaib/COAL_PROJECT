	[org 0x100]
	jmp start

	playerAttribute: dw 0x38F0,0x38AF
	enemyAttribute: dw 0x6745, 0x674E 
	wallAttribute: dw 0x10AE, 0x10AE
	trackAttribute: dw 0x7720, 0x7720


	perkAttribute: dw 0x7507, 0x7507
	perkAttribute20: dw 0x7207, 0x7207
	perkAttribute50: dw 0x7107, 0x7107

	HearthAttribute:  dw 0x0403, 0x0403
	finishAttribute: dw 0xFCDC, 0xFCDC
	
	supermanAttribute: dw 0x753
	
	outerBorderAttribute: dw  0x10AE
	innerBorderAttribute: dw 0x7720 
	stringAttribute: dw 0x09

	score: dw 0
	healthCount: dw 3
	supermanCount: dw 2
	

	finishIndex: dw 0
	playerIndexDisplay: dw 0

	maze1:db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
		db 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
		db 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1
		db 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 4, 0, 1
		db 1, 0, 1, 4, 1, 0, 0, 0, 3, 0, 0, 0, 1, 1, 1, 1
		db 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1
		db 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1
		db 1, 0, 0, 0, 0, 0, 1, 6, 3, 0, 1, 0, 1, 5, 0, 1
		db 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1
		db 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 3, 1
		db 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 4, 0, 1
		db 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1
		db 1, 0, 1, 1, 1, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 1
		db 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1
		db 1, 0, 1, 5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1
		db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
	 
	maze2:db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
		db 1, 2, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1
		db 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1
		db 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1
		db 1, 0, 1, 1, 1, 1, 3, 1, 0, 1, 1, 1, 0, 1, 1, 1
		db 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1
		db 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1
		db 1, 5, 0, 1, 0, 1, 4, 0, 0, 1, 0, 0, 0, 1, 5, 1
		db 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1
		db 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 5, 0, 0, 1
		db 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1
		db 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 1
		db 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1
		db 1, 0, 1, 6, 1, 0, 1, 4, 1, 1, 1, 1, 1, 1, 0, 1
		db 1, 0, 0, 0, 1, 0, 3, 0, 0, 0, 0, 0, 0, 1, 0, 1
		db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
	 
	maze3: db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
		db 1, 2, 0, 0, 0, 0, 0, 0, 0, 1, 5, 0, 0, 0, 0, 1
		db 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1
		db 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1
		db 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1
		db 1, 0, 0, 0, 0, 1, 5, 1, 4, 0, 0, 0, 1, 0, 0, 1
		db 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1
		db 1, 0, 0, 1, 0, 0, 0, 0, 3, 0, 0, 1, 0, 0, 0, 1
		db 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1
		db 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1
		db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1
		db 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1
		db 1, 0, 1, 6, 1, 0, 1, 1, 0, 1, 4, 1, 0, 1, 0, 1
		db 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1
		db 1, 0, 0, 0, 3, 0, 1, 3, 0, 0, 0, 0, 0, 1, 0, 1
		db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
	
	maze4: db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
		db 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1
		db 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 5, 1, 0, 1, 1
		db 1, 0, 0, 0, 1, 0, 1, 4, 1, 0, 1, 1, 1, 0, 0, 1	   
		db 1, 0, 1, 4, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1	   
		db 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1	   
		db 1, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1	   
		db 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1	   
		db 1, 0, 0, 0, 0, 0, 6, 1, 0, 1, 0, 0, 0, 0, 0, 1	   
		db 1, 0, 1, 1, 3, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1
		db 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 4, 1	   
		db 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1
		db 1, 0, 1, 1, 1, 0, 0, 0, 1, 5, 1, 0, 0, 3, 0, 1		   
		db 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1
		db 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1
		db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1

	randNum: dw 0
	tickcount:    dw   20                  ; start from 60 seconds
	tickticks:    dw   0                   ; counter for 18 ticks (1 second)

	gameOverMsg db 'Game Over!', 0
	timeUpMsg db 'Time Up!', 0
	victoryMsg db 'Congratulations! You won!', 0
	supModeMsg db 'Press space to enter into superman mode',0
	scoreMsg db 'Final Score: ', 0
	PlayerDead: db 'Player Died :(', 0

	isESCpressed dw 0
	
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
	 
		mov di, 154           
		mov word [es: di - 2], 0x073A 

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
		
		mov word [es: di - 2], 0x843A 
		singledigitLoop:				
			pop  dx                
			mov  dh, 0x84           
			mov  [es:di], dx         
			add  di, 2              
		loop singledigitLoop
	 
		mov  word[es:di], 0x0720
		
		;cmp dl, 1
		;jnz term
		;
		;dec dx
		;mov  word[es:di - 2], dx
	  
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
		cmp  ax, 0x30               ; Check if tick counter reached 18 (approximately 1 second)
		jb   skip_display_update    

		mov  word [cs:tickticks], 0 
		dec  word [cs:tickcount]    

		mov  ax, [cs:tickcount]
		cmp  ax, 0
		jge display_update         
		
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
	
	printstr:     
			push bp 
			mov  bp, sp 
			push es 
			push ax 
			push cx 
			push si 
			push di 
		
			push ds  
			pop  es                  
			mov  di, [bp+4]          
			mov  cx, 0xffff          
			xor  al, al              
			repne scasb             
			mov  ax, 0xffff          
			sub  ax, cx             
			dec  ax                  
			jz   exit               

			mov  cx, ax              
			mov  ax, 0xb800 
			mov  es, ax              
			mov  al, 80              
			mul  byte [bp+8]         
			add  ax, [bp+10]        
			shl  ax, 1               
			mov  di,ax               
			mov  si, [bp+4]          
			mov  ah, [bp+6]         
			cld                      
			
			nextchar:     
				  lodsb                    
				  stosw                    
			loop nextchar            
			exit:         
				pop  si 
				pop  cx 
				pop  di 
				pop  ax 
				pop  es 
				pop  bp 
	ret  8 

	printFinalScore:
		pusha 
		
		mov ax, [score]
		mov  bx, 10             
		mov  cx, 0               
		
		nextScoreDigit:
			mov  dx, 0              
			div  bx                 
			add  dl, 0x30           
			push dx                 
			inc  cx                 
			cmp  ax, 0  			
		jnz nextScoreDigit          

		mov  di, 2330        
		
		nextScorePos:    
			pop  dx                
			mov  dh, [stringAttribute]           
			mov  [es:di], dx         
			add  di, 2              
		loop nextScorePos            
		
		popa
	ret
	
	drawBorder:
		pusha
		mov ax, 0xb800
		mov es, ax
		mov ax, [outerBorderAttribute]
		mov di, 0                
		mov cx, 80               
		
		drawOuterTop:
			mov [es:di], ax
			add di, 2
		loop drawOuterTop
		
		mov di, 3840             
		mov cx, 80               
		drawOuterBottom:
			mov [es:di], ax
			add di, 2
		loop drawOuterBottom
		
		mov cx, 25               
		mov di, 0                
		drawOuterSides:
			mov [es:di], ax 
			mov [es: di + 2], ax
			mov bx, di
			add bx, 158               
			mov [es:bx], ax   
			mov [es:bx - 2], ax
			add di, 160               
		loop drawOuterSides
		
		mov ax, [innerBorderAttribute]
		
		mov di, 164              
		mov cx, 76               
		drawInnerTop:
			mov [es:di], ax
			add di, 2
		loop drawInnerTop
		
		mov di, 3684             
		mov cx, 76               
		drawInnerBottom:
			mov [es:di], ax
			add di, 2
		loop drawInnerBottom
		
		drawInnerLeft:
		mov cx, 23               
		mov di, 164              
		
			drawInnerLeftLoop:
				mov [es:di], ax          
				mov [es:di + 2], ax
				add di, 160              
			loop drawInnerLeftLoop	
	
		drawInnerRight:
		mov cx, 23               
		mov di, 314              
		
			drawInnerRightLoop:
				mov [es:di], ax
				mov [es:di - 2], ax
				add di, 160              
			loop drawInnerRightLoop
		popa
	ret
	
gameOverScreen:
	
	call drawBorder
    
	mov  ax, 34         
	push ax                  
	mov  ax, 10 
	push ax                  
	mov  ax, [stringAttribute]            
	push ax                   
	mov  ax, gameOverMsg 
	push ax                 
	call printstr             
	
	cmp word [isESCpressed],1
	je endGame
	
	cmp word [tickcount], 0
	je timeisUP
	
	cmp word [healthCount], 0
	jnz playerWon
	
	mov  ax, 32         
	push ax                 ;x  
	mov  ax, 12 
	push ax                 ;y   
	mov  ax, [stringAttribute]          
	push ax                  
	mov  ax,  PlayerDead
	push ax          
	call printstr
	
	jmp endGame
	
	timeisUP:
	mov  ax, 35         
	push ax                 ;x  
	mov  ax, 12 
	push ax                 ;y   
	mov  ax, [stringAttribute]          
	push ax                  
	mov  ax, timeUpMsg 
	push ax                 
	call printstr          
	jmp endGame 
	
	playerWon:
		mov  ax, 27         
		push ax                  
		mov  ax, 12 
		push ax                   
		mov  ax, [stringAttribute]             
		push ax                  
		mov  ax, victoryMsg 
		push ax                 
		call printstr  
	
	endGame:
		mov  ax, 32         
		push ax                 
		mov  ax, 14 
		push ax                   
		mov  ax, [stringAttribute]              
		push ax                 
		mov  ax, scoreMsg 
		push ax                  
		call printstr           
		call printFinalScore           
	  
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
	
	scoreBox:
		pusha
		
		mov ax, 0x07CD
		mov si, 480
		mov cx, 5
		
		; lower boundary
		looop:
			mov word[es: si], ax
			add si, 2
		loop looop	
		
		; corner
		mov word[es: si], 0x07BC	
		
		mov cx, 3
		sub si, 160
		
		;upper boundary
		looop1:
			mov word[es: si], 0x07BA
			sub si, 160
		loop looop1		
		popa
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
				cmp al, 5          ; Perk
				je draw_perk20
				cmp al, 6          ; Perk
				je draw_perk50
				
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
			
			draw_perk20:
				mov bx, perkAttribute20
				jmp draw_cell
			
			draw_perk50:
				mov bx, perkAttribute50
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
		
		pusha
		mov  ax, 21         
		push ax                 ;x  
		mov  ax, 6 
		push ax                 ;y   
		mov  ax, 0x07          
		push ax                  
		mov  ax, supModeMsg 
		push ax       
		call printstr
		popa
		
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
			
			cmp word[tickcount], 0
			jz exitProgram
			
			; if health is zero end the game
			xor bx, bx
			cmp word[cs:healthCount], bx
			jz exitProgram
			
			; if player has moved to finish end the game
			cmp di, [finishIndex]
			jz exitProgram
			
			jmp mainLoop

			movementloop:
				; Check if a key is pressed
				mov ah, 00h            
				int 16h  
				
				cmp al, 27				; escape
				jz quitGame
			
				cmp al, 'w'            
				jz moveUp        
			
				cmp al, 'a'            
				jz moveLeft 
			
				cmp al, 's'            
				jz moveDown 
			
				cmp al, 'd'            
				jz moveRight 
				
				cmp al, ' '            
				jz superman 
				
				jmp mainLoop
				
				superman:
			
					cmp word [supermanCount], 0
					jz mainLoop
					
					dec word[supermanCount]
					mov ax, 3
					mov word [healthCount], 3
					call displayHealth
					
					shl word[score], 1			
					call displayScore
					call displaySupes
				
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
		quitGame:
		mov word [isESCpressed], 1
		
		exitProgram:
			call clearMaze
			call gameOverScreen
	ret

	;----{Score and Health Update}----

	ScoreHealthUpdate:
	; es is already set to 0xB800
	; ax holds the attribute of position to move before  entering this subroutine	
		cmp ax, [perkAttribute]
		jnz perk2
		
		; add ten in score when perk collected
		add word [score], 10
		jmp l1
		
		perk2:
			
			cmp ax, [perkAttribute20]
			jnz perk3
			
			; add ten in score when perk collected
			add word [score], 20
			jmp l1
		
		perk3:
			cmp ax, [perkAttribute50]
			jnz skipScoreUpdate
			
			; add ten in score when perk collected
			add word [score], 50
			
		l1:		
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
	
	displaySupes:
		pusha                         
		mov cx, [supermanCount]         
		mov ax, [supermanAttribute]     
		mov di, 322					                
		
		mov bx, 2                    
		
		;clear the previous hearts
		clear_supes:
			mov word [es:di], 0x20      
			add di, 2
			dec bx
		jnz clear_supes


		mov di, 322	
		cmp cx, 0
		jnz display_S                       
		
		; if zero hearts left no need to display heart
		jmp ending

		display_S:
			mov [es:di], ax               
			add di, 2                     
		loop display_S           

		ending:
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
		
		cmp byte[randNum], 0            ; If randNum == 0, load maze1
		je LoadMaze1         

		cmp byte[randNum], 1            ; If randNum == 1, load maze2
		je LoadMaze2         

		cmp byte[randNum], 2            ; If randNum == 2, load maze3
		je LoadMaze3         
		
		cmp byte[randNum], 3            ; If randNum == 2, load maze3
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
			
			call scoreBox	
			call displayHealth
			call displayScore
			call displaySupes
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