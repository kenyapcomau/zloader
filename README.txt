This is software for generating a BIOS extension ROM from embedded x86 programs
that do not require any external routines other than BIOS calls. A version of
this was used to generate Etherboot ROMs.

I have no idea what is needed to make this work again. I have no use case for
it so I'm releasing it as-is.

Original README:

These are files for making a ROMable version of cvt100a, but they can be
applied to any other program that will run standalone without DOS to turn it
into a ROM firmware for PCs. To use it for another program, make a symbolic
link called $(TARGET).com in the current directory and edit the Makefile. The
files loader.asm and zloader.asm come from the Etherboot package hence the
references to it in the comments. The only difference is DOS .com images use
RELOC>>4-0x10:0x100 for the entry point instead of RELOC>>4:0.
