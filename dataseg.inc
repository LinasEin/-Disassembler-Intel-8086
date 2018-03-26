	help db "Help:",0dh,0ah,"*********************",0dh,0ah,"This is a Intel 8086 disassembler.",0dh,0ah,"*********************",0dh,0ah,"Program works with .COM files.",0dh,0ah,"To use it:",0dh,0ah, "1)Enter the name of .COM file.",0dh,0ah,"2)Enter the name of the output file (for results).",0dh,0ah,"*********************",0dh,0ah,"Made by Linas Einikis 2017",0dh,0ah,"$"
	Parametererror db "ERROR! Parameter not found!$"
	registers db "alaxclcxdldxblbxahspchbpdhsibhdi"
	sreg db"escsssds"
	opnotfound db "Not_found"
	newline db 0Dh, 0Ah
	bracket db 5Dh
	bracket1 db 5Bh
	regip dw ?
	counter dw ?
	c db 2Ch
	command db 5 dup(0)
	rmip dw ?
	tab db 9h,9h,9h,9h,9h,"Hex:"
	ff db 46h, 46h
	oo db 30h,30h
	tarp db 10 dup (0)
	tarp1 db 10 dup (0)
	prefix db "es:cs:ss:ds:"
	prefixbuff dw ?
	one db 31h
	bw db 1 dup (0)
	direction db 1 dup (0)
	byt3 db 100 dup(0)
	buffforposl dw ?
	hex db 68h
	opsize dw ?
	indirect dw ?
	axc db 61h,78h,2ch
	alc db 61h,6Ch,2ch
	addbuff db 4 dup (30h)
	ptr1 db "byte ptr word ptr "
	pposl dw ?
	rm db "[bx+si][bx+di][bp+si][bp+di][si]   [di]          [bx]   "
	rmposl db "[bx+si+[bx+di+[bp+si+[bp+di+[si  + [di  + [bp  + [bx  + "
	commands db "POP LEA LDS DEC LOOP LOOPE LOOPNE DAA DAS AAA AAS NOP CBW CWD WAIT PUSHF POPF SAHF LAHF RET RETF INT 3 INTO IRET AAM AAD XLAT LOCK REPNZ REP HLT CMC CLC STC CLI STI CLD STD JO JNO JNAE JAE JE JNE JBE JA JS JNS JP JNP JL JGE JLE JG ADD PUSH OR ADC SBB " 
					 db "SUB XOR CMP INC TEST XCHG MOV MOVSB MOVSW CMPSB CMPSW STOSB STOSW LODSB LODSW SCASB SCASW OUT LES JCXZ ROL ROR RCL RCR SHL SHR SAR NOT NEG MUL IMUL DIV IDIV CALL JMP IN AND "
	resultbuff db 100 dup(0h);pavadinimo
	databuff db 100 dup(0h);pavadinimo
	buff db 512 dup(0)
	inputf dw 0;handle
	outputf dw 0;handle
	ip dw ?
	colon db 3Ah,20h
	checkbw db 1 dup(0)
	address dw ?
	opcode db 100 dup(0)
	opip dw ?
	fileerror db "Error! (files)$"
