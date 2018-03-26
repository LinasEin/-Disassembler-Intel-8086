		;Checks for valid operation code and performs certain actions depending on the code.
		
		mov byt3,al
		and al,11100111b
		cmp al,00000111b
		je pop1
		mov al,byt3		
		and al,11111000b
		cmp al,01011000b
		je pop2
		mov al,byt3
		and al,11111111b
		cmp al,10001111b
		je pop3
		mov al,byt3 
		and al,11111100b
		cmp al,00100000b
		je and1
		mov al,byt3
		and al,11111110b
		cmp al,00100100b
		je and2
		jmp blocknext1
		
		pop1:
		lea dx, commands
		mov cx,4
		call output
		call popsr
		jmp checkend
		pop2:
		lea dx, commands
		mov cx,4
		call output
		call wreg
		jmp checkend
		pop3:
		lea dx, commands
		mov cx,4
		call output
		call readnextbyte
		call modrm
		jmp checkend
		and1:
		lea dx,[commands+420]
		mov cx,4
		call output
		call regrm
		jmp checkend
		and2:
		lea dx,[commands+420]
		mov cx,4
		call output
		call andab
		jmp checkend
		
		blocknext1:
		mov al,byt3
		and al,11111100b
		cmp al,10000000b
		je block1
		jmp blocknext2
		
		block1:
		mov al,byt3
		call checkd
		mov al,byt3
		call checkw
		call readnextbyte
		
		and al,00111000b
		cmp al, 00000000b
		je add3
		mov al, byt3
		and al,00111000b
		cmp al, 00001000b
		je or3
		mov al,byt3
		and al,00111000b
		cmp al, 00010000b
		je adc3
		mov al,byt3
		and al,00111000b
		cmp al,00011000b
		je sbb3
		mov al,byt3
		and al,00111000b
		cmp al,00100000b
		je and3
		mov al,byt3
		and al,00111000b
		cmp al,00101000b
		je sub3
		
		add3:
		lea dx, [commands+231]
		mov cx,4
		jmp block1end
		or3:
		lea dx, [commands+240]
		mov cx,4
		jmp block1end
		adc3:
		lea dx, [commands+243]
		mov cx,4
		jmp block1end
		sbb3:
		lea dx, [commands+247]
		mov cx,4
		jmp block1end
		and3:
		mov [command],41h
		mov [command+1],4eh
		mov [command+2],44h
		mov [command+3],20h
		lea dx,command
		mov cx,4
		jmp block1end
		sub3:
		lea dx, [commands+251]
		mov cx,4
		jmp block1end
		xor3:
		lea dx,[commands+255]
		mov cx,4
		jmp block1end
		cmp3:
		lea dx,[commands+259]
		mov cx,4
		block1end:
		call output
		call andrmb
		jmp checkend	
		
		blocknext2:
		mov al,byt3
		and al,11111100b
		cmp al,11010000b
		je block2
		jmp blocknext3
		block2:
		mov al,byt3
		call checkd
		mov al,byt3
		call checkw
		call readnextbyte
		and al,00111000b
		cmp al, 00000000b
		je rol1
		mov al, byt3
		and al,00111000b
		cmp al, 00001000b
		je ror1
		mov al,byt3
		and al,00111000b
		cmp al, 00010000b
		je rcl1
		mov al,byt3
		and al,00111000b
		cmp al,00011000b
		je rcr1
		mov al,byt3
		and al,00111000b
		cmp al,00100000b
		je shl1
		mov al,byt3
		and al,00111000b
		cmp al,00101000b
		je shr1
		mov al,byt3
		and al,00111000b
		cmp al,00111000b
		je sar1
		
		rol1:
		lea dx, [commands+354]
		mov cx,4
		jmp block2end
		ror1:
		lea dx, [commands+358]
		mov cx,4
		jmp block2end
		rcl1:
		lea dx, [commands+362]
		mov cx,4
		jmp block2end
		rcr1:
		lea dx, [commands+366]
		mov cx,4
		jmp block2end
		shl1:
		lea dx, [commands+370]
		mov cx,4
		jmp block2end
		shr1:
		lea dx, [commands+374]
		mov cx,4
		jmp block2end
		sar1:
		lea dx,[commands+378]
		mov cx,4
		jmp block2end
		block2end:
		call output
		call modrm
		call comma
		cmp direction,1
		je nextcl
		lea dx,one
		mov cx,1
		call output
		jmp checkend
		nextcl:
		lea dx,[registers+4]
		mov cx,2
		call output
		jmp checkend	
		
		blocknext3:
		mov al,byt3
		and al,11111110b
		cmp al,11110110b
		je block3
		jmp blocknext4
		block3:
		mov al,byt3
		call checkw
		call readnextbyte
		and al,00111000b
		cmp al, 00000000b
		je test3
		mov al, byt3
		and al,00111000b
		cmp al, 00010000b
		je not1
		mov al, byt3
		and al,00111000b
		cmp al, 00011000b
		je neg1
		mov al,byt3
		and al,00111000b
		cmp al, 00100000b
		je mul1
		mov al,byt3
		and al,00111000b
		cmp al,00101000b
		je imul1
		mov al,byt3
		and al,00111000b
		cmp al,00110000b
		je div1
		mov al,byt3
		and al,00111000b
		cmp al,00111000b
		je idiv1
		test3:
		lea dx, [commands+267]
		mov cx,5
		call output
		call andrmb
		jmp checkend
		not1:
		lea dx, [commands+382]
		mov cx,4
		jmp block3end
		neg1:
		lea dx, [commands+386]
		mov cx,4
		jmp block3end
		mul1:
		lea dx, [commands+390]
		mov cx,4
		jmp block3end
		imul1:
		lea dx, [commands+394]
		mov cx,5
		jmp block3end
		div1:
		lea dx, [commands+399]
		mov cx,4
		jmp block3end
		idiv1:
		lea dx, [commands+403]
		mov cx,5
		block3end:
		call output
		call modrm
		jmp checkend
		
		blocknext4:
		mov al,byt3
		and al,11111110b
		cmp al,11111110b
		je block4
		jmp next1
		block4:
		mov al,byt3
		call checkw
		call readnextbyte
		and al,00111000b
		cmp al, 00000000b
		je inc2
		mov al, byt3
		and al,00111000b
		cmp al, 00001000b
		je dec2
		mov al,byt3
		and al,00111000b
		cmp al,00110000b
		je push3
		jmp block4_1
		
		inc2:
		lea dx, [commands+263]
		mov cx,4
		jmp block4end
		dec2:
		lea dx, [commands+12]
		mov cx,4
		jmp block4end
		push3:
		lea dx, [commands+235]
		mov cx,5
		block4end:
		call output
		call modrm
		jmp checkend
		
		block4_1:
		mov al,byt3
		and al,00111000b
		cmp al, 00010000b
		je call2
		mov al,byt3
		and al,00111000b
		cmp al,00011000b
		je call3
		mov al,byt3
		and al,00111000b
		cmp al,00100000b
		je jmp1
		mov al,byt3
		and al,00111000b
		cmp al,00101000b
		je jmp2
		call2:
		lea dx, [commands+408]
		mov cx,5
		jmp block4_1end
		call3:
		lea dx, [commands+408]
		mov cx,5
		jmp block4_2end
		jmp1:
		lea dx, [commands+413]
		mov cx,4
		jmp block4_1end
		jmp2:
		lea dx, [commands+413]
		mov cx,4
		jmp block4_2end
		block4_1end:
		call output
		call modrm
		jmp checkend
		block4_2end:
		call output
		mov indirect,2
		call modrm
		jmp checkend
		
		next1:
		mov al,byt3
		and al,11111111b
		cmp al,10001101b
		je lea1
		mov al,byt3
		and al,11111111b
		cmp al,11000101b
		je lds1
		mov al,byt3
		and al,11111000b
		cmp al,01001000b
		je dec1
		jmp next2
		lea1:
		lea dx,[commands+4]
		mov cx,4
		call output
		call readnextbyte
		call mreg
		jmp checkend
		lds1:
		lea dx,[commands+8]
		mov cx,4
		call output
		call readnextbyte
		call mreg
		jmp checkend
		dec1:
		lea dx,[commands+12]
		mov cx,4
		call output
		call wreg
		jmp checkend
		
		next2:
		mov al,byt3
		and al,11111111b
		cmp al,11100010b
		je loop1
		mov al,byt3
		and al,11111111b
		cmp al,11100001b
		je loope1
		mov al,byt3
		and al,11111111b
		cmp al,11100000b
		je loopne1
		jmp next3
		
		loop1:
		lea dx, [commands+16]
		mov cx,5
		call output
		call shiftforloop
		jmp checkend
		loope1:
		lea dx, [commands+21]
		mov cx,6
		call output
		call shiftforloop
		jmp checkend
		loopne1:
		lea dx, [commands+27]
		mov cx,7
		call output
		call shiftforloop
		jmp checkend
		
		next3:
		mov al,byt3
		and al,11111111b
		cmp al,00100111b
		je daa1
		mov al,byt3
		and al,11111111b
		cmp al,00101111b
		je das1
		mov al,byt3
		and al,11111111b
		cmp al,00110111b
		je aaa1
		mov al,byt3
		and al,11111111b
		cmp al,00111111b
		je aas1
		mov al,byt3
		and al,11111111b
		cmp al,10010000b
		je nop1
		mov al,byt3
		and al,11111111b
		cmp al,10011000b
		je cbw1
		mov al,byt3
		and al,11111111b
		cmp al,10011001b
		je cwd1
		mov al,byt3
		and al,11111111b
		cmp al,10011011b
		je wait1
		mov al,byt3
		and al,11111111b
		cmp al,10011100b
		je pushf1
		mov al,byt3
		and al,11111111b
		cmp al,10011101b
		je popf1
		jmp next4
		
		daa1:
		lea dx, [commands+34]
		mov cx,3
		call output
		jmp checkend
		das1:
		lea dx, [commands+38]
		mov cx,3
		call output
		jmp checkend
		aaa1:
		lea dx, [commands+42]
		mov cx,3
		call output
		jmp checkend
		aas1:
		lea dx, [commands+46]
		mov cx,3
		call output
		jmp checkend
		nop1:
		lea dx, [commands+50]
		mov cx,3
		call output
		jmp checkend
		cbw1:
		lea dx, [commands+54]
		mov cx,3
		call output
		jmp checkend
		cwd1:
		lea dx, [commands+58]
		mov cx,3
		call output
		jmp checkend
		wait1:
		lea dx, [commands+62]
		mov cx,4
		call output
		jmp checkend
		pushf1:
		lea dx, [commands+67]
		mov cx,5
		call output
		jmp checkend
		popf1:
		lea dx, [commands+73]
		mov cx,4
		call output
		jmp checkend
		
		next4:
		mov al,byt3
		and al,11111111b
		cmp al,10011110b
		je sahf1
		mov al,byt3
		and al,11111111b
		cmp al,10011111b
		je lahf1
		mov al,byt3
		and al,11111111b
		cmp al,11000011b
		je ret1
		mov al,byt3
		and al,11111111b
		cmp al,11001011b
		je retf1
		mov al,byt3
		and al,11111111b
		cmp al,11001100b
		je int31
		mov al,byt3
		and al,11111111b
		cmp al,11001110b
		je into1
		mov al,byt3
		and al,11111111b
		cmp al,11001111b
		je iret1
		mov al,byt3
		and al,11111111b
		cmp al,11010100b
		je aam1
		mov al,byt3
		and al,11111111b
		cmp al,11010101b
		je aad1
		mov al,byt3
		and al,11111111b
		cmp al,11010111b
		je xlat1
		jmp next5
		
		sahf1:
		lea dx, [commands+78]
		mov cx,4
		call output
		jmp checkend
		lahf1:
		lea dx, [commands+83]
		mov cx,4
		call output
		jmp checkend
		ret1:
		lea dx, [commands+88]
		mov cx,3
		call output
		jmp checkend
		retf1:
		lea dx, [commands+92]
		mov cx,4
		call output
		jmp checkend
		int31:
		lea dx, [commands+97]
		mov cx,5
		call output
		jmp checkend
		into1:
		lea dx, [commands+103]
		mov cx,4
		call output
		jmp checkend
		iret1:
		lea dx, [commands+108]
		mov cx,4
		call output
		jmp checkend
		aam1:
		call readnextbyte
		lea dx, [commands+113]
		mov cx,3
		call output
		jmp checkend
		aad1:
		call readnextbyte
		lea dx, [commands+117]
		mov cx,3
		call output
		jmp checkend
		xlat1:
		lea dx, [commands+121]
		mov cx,4
		call output
		jmp checkend
		
		next5:
		mov al,byt3
		and al,11111111b
		cmp al,11110000b
		je lock1
		mov al,byt3
		and al,11111111b
		cmp al,11110010b
		je repnz1
		mov al,byt3
		and al,11111111b
		cmp al,11110011b
		je rep1
		mov al,byt3
		and al,11111111b
		cmp al,11110100b
		je hlt1
		mov al,byt3
		and al,11111111b
		cmp al,11110101b
		je cmc1
		mov al,byt3
		and al,11111111b
		cmp al,11111000b
		je clc1
		mov al,byt3
		and al,11111111b
		cmp al,11111001b
		je stc1
		mov al,byt3
		and al,11111111b
		cmp al,11111010b
		je cli1
		mov al,byt3
		and al,11111111b
		cmp al,11111011b
		je sti1
		jmp next6
		lock1:
		lea dx, [commands+126]
		mov cx,4
		call output
		jmp checkend
		repnz1:
		lea dx, [commands+131]
		mov cx,5
		call output
		jmp checkend
		rep1:
		lea dx, [commands+137]
		mov cx,3
		call output
		jmp checkend
		hlt1:
		lea dx, [commands+141]
		mov cx,3
		call output
		jmp checkend
		cmc1:
		lea dx, [commands+145]
		mov cx,3
		call output
		jmp checkend
		clc1:
		lea dx, [commands+149]
		mov cx,3
		call output
		jmp checkend
		stc1:
		lea dx, [commands+153]
		mov cx,3
		call output
		jmp checkend
		cli1:
		lea dx, [commands+157]
		mov cx,3
		call output
		jmp checkend
		sti1:
		lea dx, [commands+161]
		mov cx,3
		call output
		jmp checkend
		
		next6:
		mov al,byt3
		and al,11111111b
		cmp al,11111100b
		je cld1
		mov al,byt3
		and al,11111111b
		cmp al,11111101b
		je std1
		mov al,byt3
		and al,11111111b
		cmp al,01110000b
		je jo1
		mov al,byt3
		and al,11111111b
		cmp al,01110001b
		je jno1
		mov al,byt3
		and al,11111111b
		cmp al,01110010b
		je jnae1
		mov al,byt3
		and al,11111111b
		cmp al,01110011b
		je jae1
		mov al,byt3
		and al,11111111b
		cmp al,01110100b
		je je1
		mov al,byt3
		and al,11111111b
		cmp al,01110101b
		je jne1
		mov al,byt3
		and al,11111111b
		cmp al,01110110b
		je jbe1
		jmp next7
		cld1:
		lea dx, [commands+165]
		mov cx,3
		call output
		jmp checkend
		std1:
		lea dx, [commands+169]
		mov cx,3
		call output
		jmp checkend
		jo1:
		lea dx, [commands+173]
		mov cx,3
		call output
		call shiftforloop
		jmp checkend
		jno1:
		lea dx, [commands+176]
		mov cx,4
		call output
		call shiftforloop
		jmp checkend
		jnae1:
		lea dx, [commands+180]
		mov cx,5
		call output
		call shiftforloop
		jmp checkend
		jae1:
		lea dx, [commands+185]
		mov cx,4
		call output
		call shiftforloop
		jmp checkend
		je1:
		lea dx, [commands+189]
		mov cx,3
		call output
		call shiftforloop
		jmp checkend
		jne1:
		lea dx, [commands+192]
		mov cx,4
		call output
		call shiftforloop
		jmp checkend
		jbe1:
		lea dx, [commands+196]
		mov cx,4
		call output
		call shiftforloop
		jmp checkend
		
		next7:
		mov al,byt3
		and al,11111111b
		cmp al,01110111b
		je ja1
		mov al,byt3
		and al,11111111b
		cmp al,01111000b
		je js1
		mov al,byt3
		and al,11111111b
		cmp al,01111001b
		je jns1
		mov al,byt3
		and al,11111111b
		cmp al,01111010b
		je jp1
		mov al,byt3
		and al,11111111b
		cmp al,01111011b
		je jnp1
		mov al,byt3
		and al,11111111b
		cmp al,01111100b
		je jl1
		mov al,byt3
		and al,11111111b
		cmp al,01111101b
		je jge1
		mov al,byt3
		and al,11111111b
		cmp al,01111110b
		je jle1
		mov al,byt3
		and al,11111111b
		cmp al,01111111b
		je jg1
		jmp next8
		
		ja1:
		lea dx, [commands+200]
		mov cx,3
		call output
		call shiftforloop
		jmp checkend
		js1:
		lea dx, [commands+203]
		mov cx,3
		call output
		call shiftforloop
		jmp checkend
		jns1:
		lea dx, [commands+206]
		mov cx,4
		call output
		call shiftforloop
		jmp checkend
		jp1:
		lea dx, [commands+210]
		mov cx,3
		call output
		call shiftforloop
		jmp checkend
		jnp1:
		lea dx, [commands+213]
		mov cx,4
		call output
		call shiftforloop
		jmp checkend
		jl1:
		lea dx, [commands+217]
		mov cx,3
		call output
		call shiftforloop
		jmp checkend
		jge1:
		lea dx, [commands+220]
		mov cx,4
		call output
		call shiftforloop
		jmp checkend
		jle1:
		lea dx, [commands+224]
		mov cx,4
		call output
		call shiftforloop
		jmp checkend
		jg1:
		lea dx, [commands+228]
		mov cx,3
		call output
		call shiftforloop
		jmp checkend
		
		next8:
		mov al,byt3
		and al,11111100b
		cmp al,00000000b
		je add1
		mov al,byt3
		and al,11111110b
		cmp al,00000100b
		je add2
		mov al,byt3
		and al,11100111b
		cmp al,00000110b
		je push1
		mov al,byt3
		and al,11111100b
		cmp al,00001000b
		je or1
		mov al,byt3
		and al,11111110b
		cmp al,00001100b
		je or2
		mov al,byt3
		and al,11111100b
		cmp al,00010000b
		je adc1
		mov al,byt3
		and al,11111110b
		cmp al,00010100b
		je adc2
		mov al,byt3
		and al,11111100b
		cmp al,00011000b
		je sbb1
		mov al,byt3
		and al,11111110b
		cmp al,00011100b
		je sbb2
		jmp next9
		
		add1:
		lea dx,[commands+231]
		mov cx,4
		call output
		call regrm
		jmp checkend
		add2:
		lea dx,[commands+231]
		mov cx,4
		call output
		call andab
		jmp checkend
		push1:
		lea dx, [commands+235]
		mov cx,5
		call output
		call popsr
		jmp checkend
		or1:
		lea dx,[commands+240]
		mov cx,3
		call output
		call regrm
		jmp checkend
		or2:
		lea dx,[commands+240]
		mov cx,3
		call output
		call andab
		jmp checkend
		adc1:
		lea dx,[commands+243]
		mov cx,4
		call output
		call regrm
		jmp checkend
		adc2:
		lea dx,[commands+243]
		mov cx,4
		call output
		call andab
		jmp checkend
		sbb1:
		lea dx,[commands+247]
		mov cx,4
		call output
		call regrm
		jmp checkend
		sbb2:
		lea dx,[commands+247]
		mov cx,4
		call output
		call andab
		jmp checkend
		
		next9:
		mov al,byt3
		and al,11111100b
		cmp al,00101000b
		je sub1
		mov al,byt3
		and al,11111110b
		cmp al,00101100b
		je sub2
		mov al,byt3
		and al,11111100b
		cmp al,00110000b
		je xor1
		mov al,byt3
		and al,11111110b
		cmp al,00110100b
		je xor2
		mov al,byt3
		and al,11111100b
		cmp al,00111000b
		je cmp1
		mov al,byt3
		and al,11111110b
		cmp al,00111100b
		je cmp2
		mov al,byt3
		and al,11111000b
		cmp al,01000000b
		je inc1
		mov al,byt3
		and al,11111000b
		cmp al,01010000b
		je push2
		mov al,byt3
		and al,11111110b
		cmp al,10000100b
		je test1
		jmp next10
		sub1:
		lea dx,[commands+251]
		mov cx,4
		call output
		call regrm
		jmp checkend
		sub2:
		lea dx,[commands+251]
		mov cx,4
		call output
		call andab
		jmp checkend
		xor1:
		lea dx,[commands+255]
		mov cx,4
		call output
		call regrm
		jmp checkend
		xor2:
		lea dx,[commands+255]
		mov cx,4
		call output
		call andab
		jmp checkend
		cmp1:
		lea dx,[commands+259]
		mov cx,4
		call output
		call regrm
		jmp checkend
		cmp2:
		lea dx,[commands+259]
		mov cx,4
		call output
		call andab
		jmp checkend
		inc1:
		lea dx, [commands+263]
		mov cx,4
		call output
		call wreg
		jmp checkend
		push2:
		lea dx, [commands+235]
		mov cx,5
		call output
		call wreg
		jmp checkend
		test1:
		lea dx, [commands+267]
		mov cx,5
		call output
		call regrm
		jmp checkend
		
		next10:
		mov al,byt3
		and al,11111110b
		cmp al,10000110b
		je xchg1
		mov al,byt3
		and al,11111100b
		cmp al,10001000b
		je mov1
		mov al,byt3
		and al,11111000b
		cmp al,10010000b
		je xchg2
		mov al,byt3
		and al,11111111b
		cmp al,10100100b
		je movsb1
		mov al,byt3
		and al,11111111b
		cmp al,10100101b
		je movsw1
		mov al,byt3
		and al,11111111b
		cmp al,10100110b
		je cmpsb1
		mov al,byt3
		and al,11111111b
		cmp al,10100111b
		je cmpsw1
		mov al,byt3
		and al,11111111b
		cmp al,10101010b
		je stosb1
		mov al,byt3
		and al,11111111b
		cmp al,10101011b
		je stosw1
		jmp next11
		
		xchg1:
		lea dx, [commands+272]
		mov cx,5
		call output
		call regrm
		jmp checkend
		mov1:
		lea dx, [commands+277]
		mov cx,4
		call output
		call regrm
		jmp checkend
		xchg2:
		lea dx, [commands+272]
		mov cx,5
		call output
		call wreg
		call comma
		lea dx, [registers+2]
		mov cx,2
		call output
		jmp checkend
		movsb1:
		lea dx, [commands+281]
		mov cx,5
		call output
		jmp checkend
		movsw1:
		lea dx, [commands+287]
		mov cx,5
		call output
		jmp checkend
		cmpsb1:
		lea dx, [commands+293]
		mov cx,5
		call output
		jmp checkend
		cmpsw1:
		lea dx, [commands+299]
		mov cx,5
		call output
		jmp checkend
		stosb1:
		lea dx, [commands+305]
		mov cx,5
		call output
		jmp checkend
		stosw1:
		lea dx, [commands+311]
		mov cx,5
		call output
		jmp checkend
		
		next11:
		mov al,byt3
		and al,11111110b
		cmp al,10101000b
		je test2
		mov al,byt3
		and al,11111111b
		cmp al,10101100b
		je lodsb1
		mov al,byt3
		and al,11111111b
		cmp al,10101101b
		je lodsw1
		mov al,byt3
		and al,11111110b
		cmp al,10101110b
		je scasb1
		mov al,byt3
		and al,11111110b
		cmp al,10101111b
		je scasw1
		jmp next11_1
	
		test2:
		lea dx, [commands+267]
		mov cx,5
		call output
		call andab
		jmp checkend
		lodsb1:
		lea dx, [commands+317]
		mov cx,5
		call output
		jmp checkend
		lodsw1:
		lea dx, [commands+323]
		mov cx,5
		call output
		jmp checkend
		scasb1:
		lea dx, [commands+329]
		mov cx,5
		call output
		jmp checkend
		scasw1:
		lea dx, [commands+335]
		mov cx,5
		call output
		jmp checkend
		les1:
		lea dx, [commands+345]
		mov cx,4
		call output
		call regrm
		jmp checkend
		int1:
		lea dx, [commands+97]
		mov cx,4
		call output
		call readnextbyte
		call putposl
		call h
		jmp checkend
		mov al,byt3
		next11_1:
		mov al,byt3
		and al,11111111b
		cmp al,11000100b
		je les1
		mov al,byt3
		and al,11111111b
		cmp al,11001101b
		je int1
		
		next12:
		mov al,byt3
		and al,11111111b
		cmp al,11100011b
		je jcxz1
		mov al,byt3
		and al,11111110b
		cmp al,11100100b
		je in1
		mov al,byt3
		and al,11111110b
		cmp al,11100110b
		je out1
		mov al,byt3
		and al,11111111b
		cmp al,11101000b
		je call1
		mov al,byt3
		and al,11111111b
		cmp al,11101001b
		je jmp3
		mov al,byt3
		and al,11111111b
		cmp al,11101010b
		je jmp4
		jmp next13
		
		
		jcxz1:
		lea dx, [commands+349]
		mov cx,5
		call output
		call shiftforloop
		jmp checkend
		in1:
		lea dx, [commands+417] 
		mov cx,3
		call output
		call portab
		jmp checkend
		out1:
		lea dx, [commands+341]
		mov cx,4
		call output
		call portab
		jmp checkend
		call1:
		lea dx, [commands+408]
		mov cx,5
		call output
		call readnextbyte
		push ax
		call readnextbyte
		call putposl
		pop ax
		mov byt3,al
		call putposl
		jmp checkend
		jmp3:
		lea dx, [commands+413]
		mov cx,4
		call output
		call readnextbyte
		xor ah,ah
		push ax
		call readnextbyte
		call putposl
		pop ax
		mov byt3,al
		call putposl
		jmp checkend
		jmp4:
		lea dx, [commands+413]
		mov cx,4
		call output
		call bytes4
		jmp checkend
		
		next13:
		mov al,byt3
		and al,11111111b
		cmp al,10011010b
		je call4
		mov al,byt3
		and al,11111110b
		cmp al,11000110b
		je mov2
		mov al,byt3
		and al,11111111b
		cmp al,11101011b
		je jmp5
		mov al,byt3
		and al,11111110b
		cmp al,11101100b
		je in2
		mov al,byt3
		and al,11111110b
		cmp al,11101110b
		je out2
		mov al,byt3
		and al,11111101b
		cmp al,10001100b
		je mov6
		jmp next14
		call4:
		lea dx, [commands+408]
		mov cx,5
		call output
		call bytes4
		jmp checkend
		mov2:
		lea dx, [commands+277]
		mov cx,4
		call output
		call andrmb
		jmp checkend
		jmp5:
		lea dx, [commands+413]
		mov cx,4
		call output
		call shiftforloop
		jmp checkend
		in2:
		lea dx,[commands+417]
		mov cx,3
		call output
		lea dx,[registers+2]
		mov cx,2
		call output
		call comma
		lea dx,[registers+10]
		mov cx,2
		call output
		jmp checkend
		out2:
		lea dx,[commands+341]
		mov cx,4
		call output
		lea dx,[registers+10]
		mov cx,2
		call output
		call comma
		lea dx,[registers+2]
		mov cx,2
		call output
		jmp checkend
		mov6:
		lea dx, [commands+277]
		mov cx,4
		call output
		mov al,byt3
		call checkd
		call readnextbyte
		cmp direction,0
		je rmfirst
		call popsr
		call comma
		call modrm
		jmp checkend
		rmfirst:
		call modrm
		call comma
		call popsr
		jmp checkend
		
		next14:
		mov al,byt3
		and al,11110000b
		cmp al,10110000b
		je mov3
		mov al,byt3
		and al,11111100b
		cmp al,10100000b
		je mov4
		jmp notfound1
		
		mov3:
		lea dx,[commands+277]
		mov cx,4
		call output
		mov al,byt3
		and al,00001000b
		cmp al,00000000b
		je w00
		call wreg
		call comma
		call readnextbyte
		push ax
		call readnextbyte
		call putposl
		pop ax
		mov byt3,al
		call putposl
		call h
		jmp checkend
		w00:
		call whichreg
		mov di, regip
		dec di
		dec di
		lea dx, [registers+di]
		mov cx,2
		call output
		call comma
		call readnextbyte
		call putposl
		call h
		jmp checkend
		
		mov4:
		lea dx,[commands+277]
		mov cx,4
		call output
		
		mov al,byt3
		call checkd
		mov al,byt3
		call checkw
		
		cmp direction,0
		je dir0
		call pref
		call lbracket
		cmp bw,1
		jne bt3
		call readnextbyte
		push ax
		call readnextbyte
		call putposl
		pop ax
		mov byt3, al
		call putposl
		call h
		call rbracket
		jmp comman
		bt3:
		call readnextbyte
		call putposl
		call h
		call rbracket
		comman:
		call comma
		cmp bw,1
		je wd
		lea dx,[registers]
		mov cx,2
		call output
		jmp checkend
		wd:
		lea dx,[registers+2]
		mov cx,2
		call output
		jmp checkend
		dir0:
		cmp bw,1
		je wd1
		lea dx,[registers]
		mov cx,2
		call output
		jmp comman1
		wd1:
		lea dx,[registers+2]
		mov cx,2
		call output
		comman1:
		call comma
		call pref
		call lbracket
		cmp bw,1
		jne bt1
		call readnextbyte
		push ax
		call readnextbyte
		call putposl
		pop ax
		mov byt3, al
		call putposl
		call h
		call rbracket
		jmp checkend
		bt1:
		call readnextbyte
		call putposl
		call h
		call rbracket
		jmp checkend
		notfound1:
		call notfound
		
		