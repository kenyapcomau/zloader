;
; SYS_SIZE is the number of clicks (16 bytes) to be loaded.
; 0x7F00 is 0x7F000 bytes = 508kB, more than enough for current
; versions of linux which compress the kernel
;
SYSSIZE equ 2048; 2048 * 16 bytes = 32kB maximum size of .ROM file
		; If you know the size of the .ROM file that you are booting,
		; You can reduce this number to (rom_file_size+15)/16
;	bootsect.s		Copyright (C) 1991, 1992 Linus Torvalds
;	modified by Drew Eckhardt
;	modified by Bruce Evans (bde)
;
; bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
; itself out of the way to address 0x90000, and jumps there.
;
; bde - should not jump blindly, there may be systems with only 512K low
; memory.  Use int 0x12 to get the top of memory, etc.
;
; It then loads the system at SYSSEG<<4, using BIOS interrupts. 
;
; The loader has been made as simple as possible, and continuous
; read errors will result in a unbreakable loop. Reboot by hand. It
; loads pretty fast by getting whole tracks at a time whenever possible.

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
	org	0

BOOTSEG	equ 0x07C0			; original address of boot-sector

INITSEG	equ 0x9000			; we move boot here - out of the way
SYSSEG	equ 0x1000			; system loaded at SYSSEG<<4

#if 0 /* hook for debugger, harmless unless BIOS is fussy (old HP) */
	int	3
#endif
	mov	ax,CON(BOOTSEG)
	mov	ds,ax
	mov	ax,CON(INITSEG)
	mov	es,ax
	mov	cx,CON(256)
	sub	si,si
	sub	di,di
	cld
	rep
	movsw

	jmp	INITSEG:go

; ax and es already contain INITSEG

go:	mov	di,CON(0x4000-12)	; 0x4000 is arbitrary value >= length of
					; bootsect + length of setup + room for stack
					; 12 is disk parm size

; bde - changed 0xff00 to 0x4000 to use debugger at 0x6400 up (bde).  We
; wouldn''t have to worry about this if we checked the top of memory.  Also
; my BIOS can be configured to put the wini drive tables in high memory
; instead of in the vector table.  The old stack might have clobbered the
; drive table.

	mov	ds,ax
	mov	ss,ax		; put stack at INITSEG:0x4000-12.
	mov	sp,di

;	Many BIOS''s default disk parameter tables will not 
;	recognize multi-sector reads beyond the maximum sector number
;	specified in the default diskette parameter tables - this may
;	mean 7 sectors in some cases.
;
;	Since single sector reads are slow and out of the question,
;	we must take care of this by creating new parameter tables
;	(for the first disk) in RAM.  We will set the maximum sector
;	count to 36 - the most we will encounter on an ED 2.88.  
;
;	High doesn''t hurt.  Low does.
;
;	Segments are as follows: ds=es=ss=cs - INITSEG,

; cx contains 0 from rep movsw above

	push	es			; save es
	mov	es,cx			; access segment 0
	mov	bx,CON(0x78)		
	push	ds			; save ds
; 0:bx is parameter table address
#ifdef	USE_AS86
	seg es
#endif
#ifdef	USE_NASM
	es
#endif
	lds	si,[bx]			; loads ds and si

	mov	ax,CON(INITSEG)
	mov	es,ax
	mov	cl,CON(6)
; copy 12 bytes
	cld
	push	di			; keep a copy for later
	rep
; ds:si is source, es:di is dest
	movsw
	pop	di

#ifdef	USE_AS86
	seg es
#endif
#ifdef	USE_NASM
	es
#endif
	mov	byte [di+4],CON(36)	; patch sector count

	mov	ds,cx			; access segment 0
	xchg	[bx],di
	mov	si,es
	xchg	[bx+2],si
	pop	ds			; restore ds
	mov	LOC(dpoff),di		; save old parameters
	mov	LOC(dpseg),si		; to restore just before finishing
	pop	es			; restore es

; Note that 'es' is already set up.
; Also cx is 0 from rep movsw above.

	xor	ah,ah			; reset FDC 
	xor	dl,dl
	int 	0x13	

; Get disk drive parameters, specifically nr of sectors/track

#if 0

; bde - the Phoenix BIOS manual says function 0x08 only works for fixed
; disks.  It doesn''t work for one of my BIOS''s (1987 Award).  It was
; fatal not to check the error code.

	xor	dl,dl
	mov	ah,CON(0x08)		; AH=8 is get drive parameters
	int	0x13
	xor	ch,ch
#else

; It seems that there is no BIOS call to get the number of sectors.  Guess
; 36 sectors if sector 36 can be read, 18 sectors if sector 18 can be read,
; 15 if sector 15 can be read.  Otherwise guess 9.

	mov	si,CON(disksizes)	; table of sizes to try

probe_loop:
	lodsb
	cbw				; extend to word
	mov	LOC(sectors), ax
	cmp	si,CON(disksizes+4)
	jae	got_sectors		; if all else fails, try 9
	xchg	ax, cx			; cx = track and sector
	xor	dx, dx			; drive 0, head 0
	xor	bl, bl
	mov	bh,CON(2)		; address after boot sector
					;   (512 bytes from origin, es = cs)
	mov	ax,CON(0x0201)		; service 2, 1 sector
	int	0x13
	jc	probe_loop		; try next value

#endif

got_sectors:

; Restore es

	mov	ax,CON(INITSEG)
	mov	es,ax

; Print some inane message

	mov	ah,CON(0x03)		; read cursor pos
	xor	bh,bh
	int	0x10
	
	mov	cx,CON(32)
	mov	bx,CON(0x0007)		; page 0, attribute 7 (normal)
	mov	bp,CON(msg1)
	mov	ax,CON(0x1301)		; write string, move cursor
	int	0x10

; ok, we''ve written the message, now
; we want to load the system (at SYSSEG<<4)

	mov	ax,CON(SYSSEG)
	mov	es,ax		; segment of SYSSEG<<4
	call	read_it
	call	kill_motor
	call	print_nl

; Restore original disk parameters
	mov	bx,CON(0x78)
	mov	di,LOC(dpoff)
	mov	si,LOC(dpseg)
	xor	ax,ax
	mov	ds,ax
	mov	[bx],di
	mov	[bx+2],si

; after that (everything loaded), we jump to
; the .ROM file loaded directly after the bootblock:

	jmp	SYSSEG:6	; skip 2 bytes signature, 1 byte block count
				; jmp over and checksum byte

; This routine loads the system at address SYSSEG<<4, making sure
; no 64kB boundaries are crossed. We try to load it as fast as
; possible, loading whole tracks whenever we can.
;
; in:	es - starting address segment (normally SYSSEG)
;
sread:	dw 0			; sectors read of current track
head:	dw 0			; current head
track:	dw 0			; current track

read_it:
	mov	al,CON(1)
	mov	LOC(sread),al
	mov	ax,es
	test	ax,CON(0x0fff)
die:	jne	die		; es must be at 64kB boundary
	xor	bx,bx		; bx is starting address within segment
rp_read:
#ifdef __BIG_KERNEL__
#define CALL_HIGHLOAD_KLUDGE dw 0x1eff,0x220 ; call far * bootsect_kludge
				; NOTE: as86 can''t assemble this
	CALL_HIGHLOAD_KLUDGE	; this is within setup.S
#else
	mov ax,es
	sub ax,CON(SYSSEG)
#endif
	cmp ax,CON(SYSSIZE)	; have we loaded all yet?
	jbe ok1_read
	ret
ok1_read:
	mov ax,LOC(sectors)
	sub ax,LOC(sread)
	mov cx,ax
	shl cx,CON(9)
	add cx,bx
	jnc ok2_read
	je ok2_read
	xor ax,ax
	sub ax,bx
	shr ax,CON(9)
ok2_read:
	call read_track
	mov cx,ax
	add ax,LOC(sread)
	cmp ax,LOC(sectors)
	jne ok3_read
	mov ax,CON(1)
	sub ax,LOC(head)
	jne ok4_read
	inc word LOC(track)
ok4_read:
	mov LOC(head),ax
	xor ax,ax
ok3_read:
	mov LOC(sread),ax
	shl cx,CON(9)
	add bx,cx
	jnc rp_read
	mov ax,es
	add ah,CON(0x10)
	mov es,ax
	xor bx,bx
	JMP rp_read

read_track:
	pusha
	pusha	
	mov	ax,CON(0xe2e) 	; loading... message 2e = .
	mov	bx,CON(7)
 	int	0x10
	popa		

	mov	dx,LOC(track)
	mov	cx,LOC(sread)
	inc	cx
	mov	ch,dl
	mov	dx,LOC(head)
	mov	dh,dl
	and	dx,CON(0x0100)
	mov	ah,CON(2)
	
	push	dx				; save for error dump
	push	cx
	push	bx
	push	ax

	int	0x13
	jc	bad_rt
#ifdef	USE_AS86
	add	sp,CON(8)
#endif
#ifdef	USE_NASM
	add	sp, byte CON(8)
#endif
	popa
	ret

bad_rt:	push	ax				; save error code
	call	print_all			; ah = error, al = read


	xor ah,ah
	xor dl,dl
	int 0x13
	

#ifdef	USE_AS86
	add	sp,CON(10)
#endif
#ifdef	USE_NASM
	add	sp, byte CON(10)
#endif
	popa	
	JMP read_track

;	print_all is for debugging purposes.  
;	It will print out all of the registers.  The assumption is that this is
;	called from a routine, with a stack frame like
;	dx 
;	cx
;	bx
;	ax
;	error
;	ret <- sp
 
print_all:
	mov	cx,CON(5)	; error code + 4 registers
	mov	bp, sp	

print_loop:
	push	cx		; save count left
	call	print_nl	; nl for readability

	cmp	cl,CON(5)
	jae	no_reg		; see if register name is needed
	
	mov	ax,CON(0xe05 + 0x41 - 1)	; 'A' - 1
	sub	al, cl
	int	0x10

	mov	al,CON(0x58)	; 'X'
	int	0x10

	mov	al,CON(0x3A)	; ':'
	int	0x10

no_reg:
#ifdef	USE_AS86
	add	bp,CON(2)	; next register
#endif
#ifdef	USE_NASM
	add	bp, byte CON(2)	; next register
#endif
	call	print_hex	; print it
	pop	cx
	loop	print_loop
	ret

print_nl:
	mov	ax,CON(0xe0d)	; CR
	int	0x10
	mov	al,CON(0xa)	; LF
	int 	0x10
	ret

;	print_hex is for debugging purposes, and prints the word
;	pointed to by ss:bp in hexadecimal.

print_hex:
	mov	cx,CON(4)	; 4 hex digits
	mov	dx,[bp]	; load word into dx
print_digit:
	rol	dx,CON(4)	; rotate so that lowest 4 bits are used
	mov	ax,CON(0xe0f)	; ah = request, al = mask for nybble
	and	al, dl
	add	al,CON(0x90)	; convert al to ascii hex (four instructions)
	daa
	adc	al,CON(0x40)
	daa
	int	0x10
	loop	print_digit
	ret

; This procedure turns off the floppy drive motor, so
; that we enter the kernel in a known state, and
; don''t have to worry about it later.

kill_motor:
	push dx
	mov dx,CON(0x3f2)
	xor al, al
	out dx,al
	pop dx
	ret

sectors:
	dw 0

dpseg:	dw 0
dpoff:	dw 0

disksizes:
	db 36,18,15,9

msg1:
	db 13,10
#ifdef	USE_AS86
	.ascii	'Loading .ROM image from floppy'
	org	506
#endif
#ifdef	USE_NASM
	db	'Loading .ROM image from floppy'
	times	506-($-$$) db 0
#endif

rom_segment:
	dw	0x1000
rom_length:
	dw	0x0
boot_flag:
	dw	0xAA55
