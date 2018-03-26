.MODEL small
.STACK 100h
.DATA
	include dataseg.inc
.CODE
START:
	
	mov dx,@data
	mov ds,dx
	; Help message
	mov cl,[es:0080h] 
	mov	ch, 0	
	cmp	cx, 0			
	je error1
	mov	bx, 0081h
	
	find:
	cmp	[es:bx], '?/'		
	je	found			
	inc	bx			
	loop find			
	jmp	Arguments			
	
	error1:
	mov dx,offset Parametererror
	mov ah,09
	int 21h
	jmp exit
	
	ferror:
	mov dx,offset fileerror
	mov ah,09h
	int 21h
	
	exit:
	mov ax, 4c01h
	int 21h
	
	found:
	mov	ah, 9			
	mov	dx, offset help		
	int 21h
	mov ax,4c00h
	int 21h
	; Search for file names
	Arguments:
	xor si,si
	xor ch,ch
	mov	cl, [es:0080h]
	cmp cx,0
	je error1
	dec cx
	mov	bx, 0082h	
	
	argumentloop:
	mov dl, [es:bx]
	cmp dl,20h
	je readrf
	mov [databuff+si],dl	
	inc si
	inc bx
	loop argumentloop
	
	readrf:
	xor si,si
	dec cx
	inc bx
	resbuffloop:
	mov dl, [es:bx]
	inc bx
	mov [resultbuff+si],dl
	inc si
	loop resbuffloop
	
	mov ah, 3dh
	mov al,00h
	mov dx,offset databuff
	int 21h
	jc ferror
	mov inputf,ax
	
	mov ah,3ch
	xor cx,cx
	mov dx,offset resultbuff
	int 21h
	jc ferror 
	mov outputf,ax ;Output file
	
	;Start
	preparation:
	mov opsize,100h
	mov dx,offset buff
	mov cx, 200h
	call read
	cmp cx, 0 
	je eof
	mov counter,cx
	mov ip,0
	jmp checkloop
	eof:
	call dend 
	
	;Loops through the bytes till the EOF
	checkloop:
	call iposl
	call check
	mov cx,counter
	dec counter
	loop checkloop
	jmp preparation
	
	; Outputs the position of a command into the output file
	proc iposl
		call lbracket
		mov ax,opsize
		mov byt3,ah
		push ax
		call putposl
		pop ax
		mov byt3,al
		call putposl
		call rbracket
		lea dx,colon
		mov cx,2
		call output
		ret
	endp iposl
	
	proc read
		mov ah, 3fh
		mov bx, inputf
		int 21h
		jc read_error
		mov cx, ax
		ret
		read_error:
		call ferror
	endp read
	
	; Checks for the OP code
	proc check
		mov checkbw,3
		mov si,ip
		mov al,[buff+si]
		inc ip
		inc opsize
		cmp al,26h
		je es1
		cmp al,2eh
		je cs1
		cmp al,36h
		je ss1
		cmp al,3Eh
		je ds1
		mov prefixbuff,1
		jmp checkfile1
		ds1:
		mov prefixbuff,9
		jmp checkfile
		es1:
		mov prefixbuff,0
		jmp checkfile
		cs1:
		mov prefixbuff,3
		jmp checkfile
		ss1:
		mov prefixbuff,6
		
		checkfile:
		mov si,ip
		mov al,[buff+si]
		inc ip
		checkfile1:
		include opcheck.inc
		checkend:
		lea dx, newline
		mov cx,2
		call output
		ret
	endp check
	
	;This is the start of the information output. Once the OP code was found the program goes to the certain section.
	;These sections output proper registers, operands and other values.
	proc whichreg
		mov al,byt3
		and al,00000111b
		cmp al,00000000b
		je r1
		cmp al,00000001b
		je r2
		cmp al,00000010b
		je r3
		cmp al,00000011b
		je r4
		cmp al,00000100b
		je r5
		cmp al,00000101b
		je r6
		cmp al,00000110b
		je r7
		cmp al,00000111b
		je r8
		r1:
		mov regip,2
		mov rmip,0
		jmp endwhichreg
		r2:
		mov regip,6
		mov rmip,7
		jmp endwhichreg
		r3:
		mov regip,0Ah
		mov rmip,0Eh
		jmp endwhichreg
		r4:
		mov regip,0Eh
		mov rmip,15h
		jmp endwhichreg
		r5:
		mov regip,12h
		mov rmip, 1Ch
		jmp endwhichreg
		r6:
		mov regip,16h
		mov rmip, 23h
		jmp endwhichreg
		r7:
		mov regip,1Ah
		mov rmip,2Ah
		jmp endwhichreg
		r8:
		mov regip,1Eh
		mov rmip,31h
		endwhichreg:
		ret
	endp whichreg
	
	proc readnextbyte
		inc opsize
		jmp to1
		callread:
		mov dx,offset buff
		mov cx, 200h
		call read
		cmp cx,0
		je exit1
		mov ip,0
		jmp to1
		exit1:
		call dend
		to1:
		mov cx, counter
		cmp cx, 0 
		je callread
		dec cx
		mov counter,cx
		mov si,ip
		inc ip
		mov al, [buff+si]
		mov byt3,al
		mov di,opip
		mov [opcode+di],al
		inc opip
		ret
	endp readnextbyte
	
	proc putposl
		mov al,byt3
		shr al,4
		add al,30h
		cmp al,39h
		jnbe raides
		mov buffforposl,ax
		mov cx,1
		lea dx, buffforposl
		call output
		jmp next
		raides:
		add al, 7h
		mov buffforposl,ax
		mov cx,1
		lea dx, buffforposl
		call output
		next:
		xor ax, ax
		mov al,byt3
		and al,0fh
		add al,30h
		cmp al,39h
		jnbe raides1
		mov buffforposl,ax
		mov cx,1
		lea dx, buffforposl
		call output
		jmp pabaiga
		raides1:
		add al, 7h
		mov buffforposl,ax
		mov cx,1
		lea dx, buffforposl
		call output
		pabaiga:
		ret
	endp putposl
	
	proc shiftforloop
		call readnextbyte
		mov bx,opsize
		and al,10000000b
		cmp al, 10000000b
		je ffs
		mov al, byt3
		mov ah,00h
		add ax,bx
		jmp print
		ffs:
		mov al, byt3
		mov ah,0FFh
		add ax,bx
		print:
		mov byt3,ah
		push ax
		call putposl
		pop ax
		mov byt3,al
		call putposl
		ret
	endp shiftforloop
	
	proc checkw
		and al, 00000001b
		cmp al,00000001b
		je w0rd
		mov bw, 0
		jmp checkwend
		w0rd:
		mov bw,1
		checkwend:
		ret
	endp checkw
	
	proc checkd
		and al,00000010b
		cmp al,00000010b
		je regrm1
		mov direction,0
		jmp checkdend
		regrm1:
		mov direction,1
		checkdend:
		ret
	endp checkd
	
	proc reg
		mov al,byt3
		and al,00111000b
		cmp al,00000000b
		je r11
		cmp al,00001000b
		je r22
		cmp al,00010000b
		je r33
		cmp al,00011000b
		je r44
		cmp al,00100000b
		je r55
		cmp al,00101000b
		je r66
		cmp al,00110000b
		je r77
		cmp al,00111000b
		je r88
		r11:
		mov regip,2
		jmp endwhichreg1
		r22:
		mov regip,6
		jmp endwhichreg1
		r33:
		mov regip,0Ah
		jmp endwhichreg1
		r44:
		mov regip,0Eh
		jmp endwhichreg1
		r55:
		mov regip,12h
		jmp endwhichreg1
		r66:
		mov regip,16h
		jmp endwhichreg1
		r77:
		mov regip,1Ah
		jmp endwhichreg1
		r88:
		mov regip,1Eh
		endwhichreg1:
		ret
	endp reg
	
	proc popsr
		and al,00011000b
		cmp al,00000000b
		je es11
		and al,00011000b
		cmp al,00000000b
		je cs11
		and al,00011000b
		cmp al,00000000b
		je ss11
		lea dx,[prefix+9]
		mov cx,2
		call output
		jmp mov6end
		es11:
		lea dx,[prefix+0]
		mov cx,2
		call output
		jmp mov6end
		cs11:
		lea dx,[prefix+3]
		mov cx,2
		call output
		jmp mov6end
		ss11:
		lea dx,[prefix+6]
		mov cx,2
		call output
		mov6end:
		ret
	endp popsr
	
	proc wreg
		call whichreg
		mov di,regip
		lea dx, [registers+di]
		mov cx,2
		call output
		ret
	endp wreg
	
	proc pref
		cmp prefixbuff,1
		je pend
		mov di,prefixbuff
		lea dx,[prefix+di]
		mov cx,3
		call output
		pend:
		ret
	endp pref
	
	proc ptrcall
		mov di,pposl
		lea dx,[ptr1+di]
		mov cx,9
		call output
		ret
	endp ptrcall
	
	proc lbracket
		lea dx,bracket1
		mov cx,1
		call output
		ret
	endp lbracket
	
	proc rbracket
		lea dx,bracket
		mov cx,1
		call output
		ret
	endp rbracket
	
	proc rego
		lea dx,[registers+di]
		mov cx,2
		call output
		ret
	endp rego
	
	proc comma
		lea dx,c
		mov cx,1
		call output
		ret
	endp comma
	
	proc h
		lea dx,hex
		mov cx,1
		call output
		ret
	endp h
	
	proc addbytes
		call readnextbyte
		push ax
		call readnextbyte
		call putposl
		pop ax
		mov byt3,al
		call putposl
		ret
	endp addbytes
	
	proc modrm
		call whichreg
		cmp bw,0
		je bytep
		mov pposl,9
		jmp modrm2
		bytep:
		mov pposl,0
		modrm2:
		mov al, byt3
		and al, 11000000b
		cmp al, 00000000b
		jne mod011011
		mov al,byt3
		and al, 00000111b
		cmp al,00000110b
		je tsgadr
		call ptrcall
		call pref
		mod0rm:
		mov di,rmip
		lea dx,[rm+di]
		mov cx,7h
		call output
		jmp modrmend
		tsgadr:
		call ptrcall
		call pref
		call lbracket
		call addbytes
		cmp indirect,2
		je addbytes1
		jmp endpsl
		addbytes1:
		call addbytes
		endpsl:
		call rbracket
		jmp modrmend
		mod011011:
		mov al,byt3 
		and al, 11000000b
		cmp al, 11000000b
		je w01m11
		call ptrcall
		call pref
		mov al,byt3 
		and al, 11000000b
		cmp al, 01000000b
		je byte1
		byte2:
		mov di,rmip
		lea dx, [rmposl+di]
		mov cx,7h
		call output
		call addbytes
		cmp indirect,2
		je addbytes2
		jmp endpsl1
		addbytes2:
		call addbytes
		endpsl1:
		call rbracket
		jmp modrmend
		w01m11:
		cmp bw,1
		je w1m11
		mov di, regip
		dec di
		dec di
		call rego
		jmp modrmend
		w1m11:
		mov di, regip
		call rego
		jmp modrmend
		byte1:
		mov di,rmip
		lea dx, [rmposl+di]
		mov cx,7h
		call output
		call readnextbyte
		and al, 11110000b
		cmp al,10000000b
		jae ff1
		jmp oo1
		ff1:
		lea dx,ff
		mov cx,2
		call output
		jmp posl
		oo1:
		lea dx,oo
		mov cx,2
		call output
		posl:
		mov al,byt3
		call putposl
		call rbracket
		modrmend:
		ret
	endp modrm
	
	proc bytes4
		call addbytes
		call addbytes
		ret
	endp bytes4
	
	proc checkforreg
		cmp checkbw,1
		jne check2
		regi:
		mov di,regip
		call rego
		ret
		check2:
		cmp checkbw,0
		jne regi
		mov di,regip
		dec di
		dec di
		call rego
		ret
	endp checkforreg
	
	proc mreg
		call reg
		call checkforreg
		endas:
		call comma
		mov al,byt3
		call modrm
		ret
	endp mreg
	
	proc regm
		mov al,byt3
		push ax
		call modrm
		pop ax
		mov byt3,al
		call comma
		call reg
		call checkforreg
		regmend:
		ret
	endp regm
	
	proc andab
		mov al,byt3
		call checkw
		cmp bw,0
		je b
		mov di,2
		call rego
		call comma
		call addbytes
		jmp andabend
		b:
		mov di,0
		call rego
		call comma
		call readnextbyte
		call putposl
		andabend:
		lea dx,hex
		mov cx,1
		call output
		ret
	endp andab
	
	proc portab
		call checkw
		mov al,byt3
		call checkd
		cmp direction,0
		jne out11
		cmp bw,0
		je b1
		lea dx,axc
		mov cx,3
		call output
		jmp port
		b1:
		lea dx,alc
		mov cx,3
		call output
		port:
		call readnextbyte
		call putposl
		jmp portabend
		out11:
		call readnextbyte
		call putposl
		call comma
		cmp bw,0
		je b11
		mov di,2
		call rego
		jmp portabend
		b11:
		mov di,0
		call rego
		portabend:
		ret
	endp portab
	
	proc regrm
		mov al,byt3
		call checkd
		mov al,byt3
		call checkw
		call readnextbyte
		cmp bw,0
		je bait
		mov checkbw,1
		jmp toliau
		bait:
		mov checkbw,0
		toliau:
		cmp direction,0
		je rrm
		call mreg
		jmp andregrmend
		rrm:
		call regm
		andregrmend:
		ret
	endp regrm
	
	proc andrmb
		call modrm
		call comma	
		cmp direction,0
		je s0
		cmp bw,0
		je w0
		call readnextbyte
		;call lbracket
		and al, 11110000b
		cmp al,10000000b
		jae ff2
		oo2:
		lea dx,oo
		mov cx,2
		call output
		jmp posl1
		ff2:
		lea dx,ff
		mov cx,2
		call output
		jmp posl1
		posl1:
		mov al,byt3
		call putposl
		jmp andrmbend
		w0:
		call readnextbyte
		call putposl
		jmp andrmbend
		s0:
		cmp bw,1
		je w1
		call readnextbyte
		call putposl
		jmp andrmbend
		w1:
		call addbytes
		andrmbend:
		call h
		ret
	endp andrmb
	
	proc notfound
		lea dx,opnotfound
		mov cx,0Bh
		call output
		ret
	endp notfound
	
	proc output
		mov ah,40h
		xor al,al
		mov bx,outputf
		int 21h
		ret
	endp output
	
	dend:
	mov ax, 4c00h
	int 21h
	
	End START

