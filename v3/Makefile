name = "solve"
test_data_name = "generate_test_data"


default: bnr;


build:
	@fpc ${name}$$N.dpr -obin_${name}$$N -ap -CO -Mdelphi -Un

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
	rm -rvI ./*.ppu ./*/*.ppu ./*/*/*.ppu;\
	rm -rvI ./*.o ./*/*.o ./*/*/*.o; \
	exit 0;

# Правила для генерации тестовых данных
build_test_data:
	@fpc src/${test_data_name}.pas -obin_${test_data_name} -ap -CO -Mdelphi -Un

run_test_data:
	@./src/bin_${test_data_name}

generate_test_data: build_test_data run_test_data clean
	@echo "\n\n---===---\nTest data generation completed\n---===---\n\n"
