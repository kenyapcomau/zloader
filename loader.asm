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
blockmove:
        mov	si,CON(_body-_main)	; offset of _body
	/* fall thru */
#else	/* BOOTROM */
	mov	si,CON(0x100+_body-_main)
#endif	/* BOOTROM */
; Relocate body of boot code to RELOC:0
	cld
	xor	di,di			; di = 0
	mov	ax,CON(RELOC>>4)
        mov     es,ax
#ifdef	BOOTROM
	xor	cx,cx
	mov	ax,cs
	mov	ds,ax			; for the ref to size below
	mov	ch,LOC(size-_main)
#else	/* BOOTROM */
	mov	cx,CON((_main-_body+COMSIZE)/2)
#endif	/* BOOTROM */
	push	cx			; save cx before decrementing
#ifdef	USE_AS86
	seg	cs
#endif
#ifdef	USE_NASM
	cs
#endif
        rep
        movsw

; Save ROMs CS and length in floppy boot block before jumping to relocated
; code
	pop	cx
	push	ds
	mov	ax,CON(FLOPPY_SEGMENT)
	mov	ds,ax
	mov	ax,cs
	mov	LOC(ROM_SEGMENT),ax
	mov	LOC(ROM_LENGTH),cx
	pop	ds

; Change stack
	mov	bx,CON(RELOC>>4)	;  new ss
	mov	ds,bx			;  new ds

	int	0x12			;  get conventional memory size in KB
	shl	ax,CON(6)
	sub	ax,bx			
	shl	ax,CON(4)		;  new sp
	cli
	mov	ss,bx
	mov	sp,ax
#ifdef	ENTRYPOINT
	call	ENTRYPOINT
#else
	call	RELOC>>4:0
#endif
	int	0x19

; The body of etherboot is attached here at build time
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
_body:
