name = "solve"


default: bnr;


build:
	@fpc src/${name}$$N.dpr -o./bin_${name}$$N -ap -CO -Mdelphi -Un

run:
	@./bin_${name}$$N
	
bnr:
	@echo "\n\n---===---\nbuilding... \n---===---\n\n"
	
	@set +e; \
	make build; \
	EXIT_CODE=$$?; \
	make clean; \
	exit $$EXIT_CODE;

	@echo "\n\n---===---\nbuild done \ntrying to run:\n---===---\n\n"
	@make run

clean:
	@set +e;\
	rm -rv ./ppaslink.sh;\
	rm -rv ./symbol_order.fpc;\
	rm -rv ./link*.res;\
	rm -rv ./*.ppu ./*/*.ppu ./*/*/*.ppu;\
	rm -rv ./*.o ./*/*.o ./*/*/*.o; \
	exit 0;
