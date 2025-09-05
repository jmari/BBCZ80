
DOCKER_IMG = z88dk/z88dk
DOCKER_RUN = docker run -i -u $(shell id -u):$(shell id -g) -v .:/src -w /src $(DOCKER_IMG)
AS = $(DOCKER_RUN) z88dk-z80asm 
SRCS = dist.asm main.asm exec.asm eval.asm asmb.asm math.asm hook.asm cmos.asm data.asm


bbcbasic.com: $(SRCS)
	
	$(AS) -l -m $(SRCS) -o$@ 

clean:
	rm -f *.o *.err *.lis *.map *.com *.bin
