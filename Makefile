GCC=		gcc
CPP=		gcc -E
CFLAGS+=	-DRELOC=$(RELOCADDR)
CPPFLAGS+=	-DUSE_NASM -DBOOTROM -DNO_PCI_PNP_HEADER -DENTRYPOINT='(RELOC>>4)-0x10:0x100'
LCONFIG+=	-DSTACKADDR=0x98000-$(RELOCADDR)
ASM=		nasm
CLEANFILES=	lzhuf makerom *.tmp *.bin *.rom *.lzrom

TARGET=cvt100
RELOCADDR=0x90000

.SUFFIXES:	.asm .s .bin .rom .lzrom .lzrom

all:	$(TARGET).rom $(TARGET).lzrom floppyload.bin

$(TARGET).rom:	$(TARGET).com makerom rloader.bin
	cat rloader.bin $(TARGET).com > $(TARGET).rom
	./makerom -i '$(TARGET).rom' $(TARGET).rom

$(TARGET).lzrom:	$(TARGET).com lzhuf makerom rzloader.bin
	./lzhuf e $(TARGET).com $(TARGET).tmp
	cat rzloader.bin $(TARGET).tmp > $(TARGET).lzrom
	./makerom -i '$(TARGET).lzrom' $(TARGET).lzrom

lzhuf:	lzhuf.c
	$(GCC) -DENCODE -DDECODE -DMAIN -DVERBOSE -o $@ lzhuf.c

makerom: makerom.c
	$(GCC) -o $@ makerom.c

rloader.bin:	loader.asm loader.inc
	$(CPP) $(CPPFLAGS) $(LCONFIG) $(CFLAGS) -x assembler-with-cpp -o $*.s $<
	$(ASM) -f bin -o $*.bin $*.s
	$(RM) -f $*.s

rzloader.bin:	zloader.asm loader.inc
	$(CPP) $(CPPFLAGS) $(LCONFIG) $(CFLAGS) -x assembler-with-cpp -o $*.s $<
	$(ASM) -f bin -o $*.bin $*.s
	$(RM) -f $*.s

floppyload.bin:	floppyload.asm
	$(CPP) $(CPPFLAGS) $(LCONFIG) $(CFLAGS) -x assembler-with-cpp -o $*.s $<
	$(ASM) -f bin -o $*.bin $*.s
	$(RM) -f $*.s

clean:
	$(RM) $(CLEANFILES)
