
DOCKER_IMG = z88dk/z88dk
DOCKER_RUN = docker run -i -u $(shell id -u):$(shell id -g) -v .:/src -w /src $(DOCKER_IMG)
AS = $(DOCKER_RUN) z88dk-z80asm 
#version estatica vdudrv.asm se exluye 
SRCS = dist.asm cmos.asm mapper.asm vducmd.asm execloop.asm main.asm exec.asm eval.asm asmb.asm  math.asm data.asm
SRCSB = ./extensions/grxextbios.asm math.asm ./extensions/luttables.asm ./extensions/vdpsub.asm ./extensions/vduemu.asm 
SRCSC = ./extensions/ld.asm mapper.asm 

bbcbasic.com: $(SRCS)
	$(AS) -s -m -l -mz80 -b $(SRCS) -o$@ 

grx.lib: $(SRCSB)
	$(AS) -s -m -l -mz80 -b $(SRCSB) -o$@
#load extension cmd 
ld.com: $(SRCSC)
	$(AS) -s -m -l -mz80 -b $(SRCSC) -o$@


clean:
	rm -f *.o *.err *.lis *.map *.com *.bin *.sym *.def
	rm -f ./extensions/*.o ./extensions/*.err ./extensions/*.lis ./extensions/*.map ./extensions/*.com ./extensions/*.bin ./extensions/*.sym ./extensions/*.def