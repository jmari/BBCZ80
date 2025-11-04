
DOCKER_IMG = z88dk/z88dk
DOCKER_RUN = docker run -i -u $(shell id -u):$(shell id -g) -v .:/src -w /src $(DOCKER_IMG)
AS = $(DOCKER_RUN) z88dk-z80asm 
SRCS = dist.asm cmos.asm vdudrv.asm main.asm exec.asm eval.asm asmb.asm  math.asm data.asm
SRCST = testvdp.asm math.asm data.asm

bbcbasic.com: $(SRCS)

	$(AS) -s -m -l -mz80 -b $(SRCS) -o$@ 

testvdp.com: $(SRCST)
	
	$(AS) -s -m -l -mz80 -b $(SRCST) -o$@

clean:
	rm -f *.o *.err *.lis *.map *.com *.bin *.sym *.def
