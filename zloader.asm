; LZHuf (LZSS) Decompressing boot loader for ROM images
;
; this code is based on the work of Haruyasu Yoshizaki and Haruhiko Okumura
; who implemented the original compressor and decompressor in C code
;
; Copyright 1997 by M. Gutschke <gutschk@math.uni-muenster.de>
;
; Compression pays off, as soon as the uncompressed image is bigger than
; about 1.5kB. This assumes an average compressibility of about 60%.

; At entry, the processor is in 16 bit real mode and the code is being
; executed from an address it was not linked to. Code must be pic and
; 32 bit sensitive until things are fixed up.

#include "loader.inc"

#ifdef	USE_AS86
#define	CON(x)	*x
#define	LOC(x)	x
#define	JMP	jmp
	.text
#endif
#ifdef	USE_NASM
#define	CON(x)	x
#define	LOC(x)	[x]
#define	JMP	jmp short
	section	.text
#endif
_main:
#ifdef	BOOTROM
	dw	0xAA55			; BIOS extension signature
size:	db	0			; number of 512 byte blocks
					; = number of 256 word blocks
					; filled in by makerom program
	JMP	over			; skip over checksum
	db	0			; checksum 
#ifdef	NO_PCI_PNP_HEADER
	JMP	blockmove		; alternate entry point +6
					; used by floppyload
#else	/* NO_PCI_PNP_HEADER */
#ifdef	USE_AS86
	br	blockmove		; alternate entry point +6
					; used by floppyload
	.ascii	'Etherboot'		; Might as well put something here
	rmb	0x18-(*-_main)
#endif
#ifdef	USE_NASM
	jmp	blockmove		; alternate entry point +6
					; used by floppyload
	db	'Etherboot'		; Might as well put something here
	times	0x18-($-_main) db 0
#endif
	dw	PCI			; offset to pci data structure
	dw	PnP			; offset to PnP expansion header

PCI:
#ifdef	USE_AS86
	.ascii	'PCIR'			; signature
#endif
#ifdef	USE_NASM
	db	'PCIR'			; signature
#endif
	dw	0x8086			; vendor id (hardcode as Intel)
					; should be filled by makerom? 
	dw	0x1229			; device ID Intel 82557
					; should be filled by makerom?
	dw	0x0000			; pointer to vital product data
	dw	0x0018			; PCI data structure length 
	db	0			; PCI data structure revision
	db	0			; Class code byte 1
	db	0			; Class code byte 2
	db	0x02			; Class code byte 3 (from hexdumping
					; Intel bootrom image)
	dw	0x0000			; Image length same as offset 02h 
	dw	0x0001			; revision level of code /data
	db	0			; code type
	db	0x80			; indicator (from hexdumping 
					; Intel bootrom image)
	dw	0x0000			; reserved
      
PnP:	
#ifdef	USE_AS86
	.ascii	'$PnP'			; signature
#endif
#ifdef	USE_NASM
	db	'$PnP'			; signature
#endif
	db	0x01			; structure revision
	db	0x02			; length (in 16 byte increments)
	dw	0x0000			; offset of next header
	db	0			; Reserved
	db	0			; checksum filled by makerom
	dd	0x00000000		; Device identifier
	dw	0x0000			; pointer to manufacturer str
	dw	0x0000			; pointer to product name
	db	0			; device type code byte 1
	db	0			; device type code byte 2
	db	2			; device type code byte 3 (from 
					; hexdumping Intel bootrom image)
	db	0x14			; device indictors (from 
					; hexdumping Intel bootrom image)
	dw	0x0000			; boot connection vector
	dw	0x0000			; disconnect vector 
	dw	over			; bootstrap entry vector 
	dw	0x0000			; reserved
	dw	0x0000			; static resource information vector
#endif
over:
#ifndef	NOINT19H
	push	ax
	push	ds
	xor	ax,ax
	mov	ds,ax			; access first 64kB segment
	mov	ax,LOC(SCRATCHVEC+4)	; check if already installed
	cmp	ax,CON(MAGIC)		; check magic word
	jz	installed
	mov	ax,LOC(INT19VEC)	; hook into INT19h
	mov	LOC(SCRATCHVEC),ax
	mov	ax,LOC(INT19VEC+2)
	mov	LOC(SCRATCHVEC+2),ax
	mov	ax,CON(start19h)
	mov	LOC(INT19VEC),ax
	mov	ax,cs
	mov	LOC(INT19VEC+2),ax
	mov	ax,CON(MAGIC)		; set magic word
	mov	LOC(SCRATCHVEC+4),ax
installed:
	pop	ds
	pop	ax
	retf
start19h:				; clobber magic id, so that we will
	xor	ax,ax			;   not inadvertendly end up in an
	mov	ds,ax			;   endless loop
	mov	LOC(SCRATCHVEC+4),ax
	mov	ax,LOC(SCRATCHVEC+2)	; restore original INT19h handler
	mov	LOC(INT19VEC+2),ax
	mov	ax,LOC(SCRATCHVEC)
	mov	LOC(INT19VEC),ax
#endif	/* NOINT19H */
	/* fall thru */
blockmove:
#ifdef	MOVEROM	
; Relocate body of boot code to MOVEROM:0 and execute there
	cld
	mov	ax,CON(MOVEROM>>4)
        mov     es,ax
	xor	di,di			; di = 0
	xor	si,si			; si = 0
	xor	cx,cx
	mov	ax,cs
	mov	ds,ax			; for the ref to size below
	mov	ch,LOC(size-_main)
	push	cx			; save cx before decrementing
#ifdef	USE_AS86
	seg	cs			; override, source = cs
#endif
#ifdef	USE_NASM
	cs				; override, source = cs
#endif
        rep
        movsw

; Save ROMs CS and length in floppy boot block
	pop	cx
	push	ds
	mov	ax,CON(FLOPPY_SEGMENT)
	mov	ds,ax
	mov	ax,cs
	mov	LOC(ROM_SEGMENT),ax
	mov	LOC(ROM_LENGTH),cx
	pop	ds

	jmp	MOVEROM>>4:move
move:
#else
; Save ROMs CS and length in floppy boot block
	xor	cx,cx
	mov	ch,LOC(size-_main)
	push	ds
	mov	ax,CON(FLOPPY_SEGMENT)
	mov	ds,ax
	mov	ax,cs
	mov	LOC(ROM_SEGMENT),ax
	mov	LOC(ROM_LENGTH),cx
	pop	ds
#endif	/* MOVEROM */
#endif	/* BOOTROM */

; ds == cs
	mov	ax,cs
	mov	ds,ax
; Change stack
	mov	ax,CON((RELOC-SCRATCH)>>4); first 32kB -> scratch space
	mov	es,ax			;  new es
	mov	bx,CON(RELOC>>4)	;  new ss
	int	0x12			;  get conventional memory size in KB
	shl	ax,CON(6)
	sub	ax,bx			
	shl	ax,CON(4)		;  new sp
	cli
	mov	ss,bx
	mov	sp,ax


;----------------------------------------------------------------------------

; INIT -- initializes all data structures
; ====

init:	cld
#ifdef	BOOTROM
	mov	si,CON(dcodrle-_main)	; uncompress run length encoded
#else	/* BOOTROM */
	mov	si,CON(0x100+dcodrle-_main)
#endif	/* BOOTROM */
	mov	di,CON(dcode)		;   lookup table for codes
	mov	dl,CON(6)
	mov	dh,CON(0x20)
	xor	bh,bh
init0:	lodsb
	mov	bl,al
init1:	mov	cl,dh
	xor	ch,ch
	mov	al,bh
	rep
	stosb
	inc	bh
	dec	bl
	jnz	init1
	shr	dh,1
	dec	dl
	jnz	init0
	mov	bl,CON(1)		; uncompress run length encoded
	mov	bh,CON(6)		;   lookup table for code lengths
init2:	lodsb
	mov	cl,al
	xor	ch,ch
	mov	al,bl
	rep
	stosb
	inc	bl
	dec	bh
	jnz	init2
	mov	ax,es			; we no longer have to access static
	mov	ds,ax			;    data, so set segment accordingly
	mov	cx,CON(NCHAR)		; set all frequencies of leaf nodes
	mov	ax,CON(1)		;   to one
	rep
	stosw
	mov	si,CON(freq)
	mov	cx,CON(ROOT+1-NCHAR)
init3:	lodsw				; update frequencies of non-leaf nodes
	mov	bx,ax
	lodsw
	add	ax,bx
	stosw
	loop	init3
	mov	ax,CON(0xFFFF)
	stosw				; sentinel with infinite frequency
	mov	cx,CON(NCHAR)
	mov	ax,CON(TABLESZ)
init4:	stosw				; update "son" pointers for leaf nodes
	inc	ax
	loop	init4
	mov	cx,CON(ROOT+1-NCHAR)
	xor	ax,ax
init5:	stosw				; update "son" ptrs for non-leaf nodes
	add	ax,CON(2)
	loop	init5
	mov	cx,CON(ROOT+1-NCHAR)
	mov	ax,CON(NCHAR)
init6:	stosw				; update "parent" ptrs for non-leaf nd.
	stosw
	inc	ax
	loop	init6
	mov	cx,CON(NCHAR)
	xor	ax,ax
	stosw				; root node has no parent
init7:	stosw				; update "parent" ptrs for leaf nodes
	inc	ax
	loop	init7
	xor	ax,ax
	stosb				; clear getlen
	stosw				; clear getbuf
	mov	al,CON(0x20)		; fill text buffer with spaces
	mov	di,CON(spaces)
	mov	cx,CON(BUFSZ-LOOKAHEAD)
	rep
	stosb
	/* fall thru */


;----------------------------------------------------------------------------

; MAIN -- reads compressed codes and writes decompressed data
; ====

#ifdef	BOOTROM
	mov	si,CON(compressed-_main); get length of compressed data stream
#else	/* BOOTROM */
	mov	si,#0x100+compressed-_main	; com offset is 0x100
#endif	/* BOOTROM */
	mov	di,CON(uncompressed)
#ifdef	USE_AS86
	seg	cs
#endif
#ifdef	USE_NASM
	cs
#endif
	lodsw
	mov	cx,ax
	lodsw				; cannot do more than 64k anyways
main1:	push	cx
	call	dcdchr			; decode one code symbol
	or	ah,ah			; test if 8bit character
	jnz	main2
	stosb				; store verbatim
	pop	cx
	loop	main1			; proceed with next compressed code
	JMP	done			; until end of input is detected
main2:	push	ax
	call	dcdpos			; compute position in output buffer
	mov	ax,si
	sub	bx,di
	not	bx
	mov	si,bx			; si := di - dcdpos() - 1
	pop	cx
	sub	cx,CON(255-THRESHOLD)	; compute length of code sequence
	mov	dx,cx
	rep
	movsb
	mov	si,ax
	pop	cx
	sub	cx,dx			; check end of input condition
	jnz	main1			; proceed with next compressed code
done:
	mov	ax,CON(RELOC>>4)	; set ds then call etherboot
	mov	ds,ax
#ifdef	ENTRYPOINT
	call	ENTRYPOINT		;  call Etherboot body
#else
	call	RELOC>>4:0
#endif
	int	0x19

;----------------------------------------------------------------------------


; GETBIT -- gets one bit pointed to by DS:SI
; ======
; 
; changes: AX,CX,DL

getbit:	mov	cl,CON(8)
	mov	dl,LOC(getlen)		; compute number of bits required
	sub	cl,dl			;   to fill read buffer
	jae	getbit1
	mov	ax,LOC(getbuf)		; there is still enough read ahead data
	JMP	getbit2
#ifdef	USE_AS86
getbit1:seg	cs			; get next byte from input stream
#endif
#ifdef	USE_NASM
getbit1:cs				; get next byte from input stream
#endif
	lodsb
	xor	ah,ah
	shl	ax,cl			; shift, so that it will fit into
	mov	cx,LOC(getbuf)		;   read ahead buffer
	or	ax,cx
#ifdef	USE_AS86
	add	dl,CON(8)		; update number of bits in buffer
#endif
#ifdef	USE_NASM
	add	dl,byte CON(8)		; update number of bits in buffer
#endif
getbit2:mov	cx,ax
	shl	cx,1			; extract one bit from buffer
	mov	LOC(getbuf),cx
	dec	dl
	mov	LOC(getlen),dl		; and update number of bits
	shl	ax,1			; return in carry flag
	ret

	
;----------------------------------------------------------------------------

; DCDPOS -- decodes position in textbuffer as pointed to by DS:SI, result in BX
; ======
;
; changes: AX,BX,CX,DX

dcdpos:	mov	bx,CON(0x0800)
dcdpos1:shl	bl,1			; read one byte
	call	getbit
	jnc	dcdpos2
	inc	bl
dcdpos2:dec	bh
	jnz	dcdpos1
	mov	dh,bl			; read length of code from table
	xor	bh,bh
	mov	cl,[bx+dlen]
	xor	ch,ch
	mov	bl,[bx+dcode]		; get top six bits from table
	shl	bx,1
	shl	bx,1
	shl	bx,1
	shl	bx,1
	shl	bx,1
	shl	bx,1
dcdpos3:push	cx			; read the rest from the input stream
	shl	dh,1
	call	getbit
	jnc	dcdpos4
	inc	dh
dcdpos4:pop	cx
	loop	dcdpos3
	and	dh,CON(0x3f)		; combine upper and lower half of code
	or	bl,dh
	ret
	

;----------------------------------------------------------------------------

; DCDCHR -- decodes one compressed character pointed to by DS:SI
; ======
;
; changes: AX,BX,CX,DX

dcdchr:	mov	bx,CON(ROOT)		; start at root entry
	shl	bx,1
	mov	bx,[bx+son]
dcdchr1:call	getbit			; get a single bit
	jnc	dcdchr2
	inc	bx			; travel left or right
dcdchr2:shl	bx,1
	mov	bx,[bx+son]
	cmp	bx,CON(TABLESZ)		; until we come to a leaf node
	jb	dcdchr1
	mov	ax,bx
	sub	ax,CON(TABLESZ)
	/* fall thru */

;


; UPDATE -- updates huffman tree after incrementing frequency for code in BX
; ======
; 
; changes: BX,CX,DX

update:	; we do not check whether the frequency count has overrun.
	; this will cause problems for large files, but be should be fine
	; as long as the compressed size does not exceed 32kB and we
	; cannot do more than this anyways, because we load into the
	; upper 32kB of conventional memory
	push	si
	push	ax
	shl	bx,1
	mov	bx,[bx+parent]
update1:shl	bx,1
	mov	dx,[bx+freq]
	inc	dx			; increment frequency count by one
	mov	[bx+freq],dx
	mov	si,bx
	add	si,CON(freq+2)
	lodsw				; check if nodes need reordering
	cmp	dx,ax
	jbe	update5
update2:lodsw
	cmp	ax,dx
	jb	update2
	mov	cx,[si-4]
	mov	[bx+freq],cx		; swap frequency of entries
	mov	[si-4],dx
	mov	ax,si			; compute index of new entry
	sub	ax,CON(freq+4)
	mov	dx,ax
	shr	ax,1
	mov	cx,[bx+son]		; get son of old entry
	mov	si,cx
	add	si,si
	mov	[si+parent],ax		; and update the ptr to new parent
	cmp	cx,CON(TABLESZ)
	jae	update3			; do this for both branches
	mov	[si+parent+2],ax	;   if not a leaf node
update3:mov	si,dx
	mov	dx,[si+son]		; get son of new entry
	mov	[si+son],cx		; update its contents
	mov	si,dx
	add	si,si
	mov	cx,bx
	shr	cx,1
	mov	[si+parent],cx		; and update the ptr to new paren
	cmp	dx,CON(TABLESZ)
	jae	update4			; do this for both branches
	mov	[si+parent+2],cx		;   if not a leaf node
update4:mov	[bx+son],dx		; update son of old entry
	mov	bx,ax			; continue with new entry
	shl	bx,1
update5:mov	bx,[bx+parent]		; continue with parent
	or	bx,bx
	jnz	update1			; until we found the root entry
	pop	ax
	pop	si
	ret


;----------------------------------------------------------------------------

; constant data. this part of the program resides in ROM and cannot be
; changed

; run length encoded tables will be uncompressed into the bss segment
; take care with any symbols here for .com files to add 0x100 to address

dcodrle:db	0x01,0x03,0x08,0x0C,0x18,0x10
dlenrle:db	0x20,0x30,0x40,0x30,0x30,0x10

; Force 4 byte alignment
#ifdef	USE_AS86
	if	((*-_main)&3) != 0
	rmb	3-((*-_main)&3)
	db	0
	endif
#endif
#ifdef	USE_NASM
	%if	(($-_main)&3) != 0
	times	3-(($-_main)&3) db 0
	db	0
	%endif
#endif
compressed:
; compressed data will be attached here


;----------------------------------------------------------------------------

; variable data segment (bss)
; this segment will always be found at 0x90000 (i.e. at RELOC - SCRATCH)

; do not change the order or the sizes of any of the following tables
; the initialization code makes assumptions on the exact layout of the
; data structures...


; lookup table for index into buffer of recently output characters

dcode	equ	0


; lookup table for length of code sequence from buffer of recent characters

dlen	equ	dcode+256


; table with frequency counts for all codes
freq	equ	dlen+256


; pointer to child nodes
son	equ	freq+2*(TABLESZ+1)


; the first part of this table contains all the codes (0..TABLESZ-1)
; the second part contains all leaf nodes             (TABLESZ..   )
parent	equ	son+2*(TABLESZ)


; temporary storage for extracting bits from compressed data stream
getlen	equ	parent+2*(TABLESZ+NCHAR)
getbuf	equ	getlen+1


; install our stack pointer somewhere out of the way
stack	equ	SCRATCH-BUFSZ+LOOKAHEAD


; the initial buffer has to be filled with spaces

spaces	equ	stack


; uncompressed data will be written to address 0x98000

uncompressed	equ	SCRATCH
