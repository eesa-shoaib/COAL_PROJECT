[org 0x100]
jmp start

clrsrc:
		
	mov ax, 0xb800
	mov es, ax
	mov ax, 0x20
	xor di, di
	mov cx, 2000
	cld
	rep stosw
	
	ret

maze: dw 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1 
		dw 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1
		dw 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1
		dw 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1
		dw 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1
		dw 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
		dw 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1	
		dw 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1
		dw 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1
		dw 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1
		dw 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1
		dw 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1
		dw 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1
		dw 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1
		dw 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1		
		dw 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0 

	
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
	xor di, di
	
	mov cx, 256			; as the dimension of maze is 16*16
	mov si, [bp + 4]
	xor di, di
	xor dx, dx
	
MazeArrayLoop:
	
	
	cmp dx, 32 
	jnz drawing
	
	sub di, 32
	add di, 160
	mov dx, 0
	
drawing:	
	cmp word[si], 0
	jz blankspace
	
	mov word [es:di],0x10AE
	add di, 2
	add dx, 2
	add si, 2
	call delay
	
	loop MazeArrayLoop
	jmp terminate
	
blankspace:
	mov word [es:di],0x7720
	add di, 2
	add dx, 2
	add si, 2
	call delay
	
	loop MazeArrayLoop

terminate:	

	popa	
	pop bp
	ret 2


start:
	call clrsrc
	
	push maze
	call LoadMaze
	
	mov ax, 0x4c00
	int 0x21