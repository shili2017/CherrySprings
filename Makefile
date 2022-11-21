BUILD_DIR = ./build
SRC = src/main/scala/$(wildcard *.scala)

verilog: $(SRC)
	@mkdir -p $(BUILD_DIR)
	sbt "run -td $(BUILD_DIR)"
	@mv *.v $(BUILD_DIR)
	@mv firrtl_black_box_resource_files.f $(BUILD_DIR)

emu:
	cd difftest && $(MAKE) WITH_DRAMSIM3=1 EMU_TRACE=1 WITH_CHISELDB=0 emu -j

clean:
	-rm -rf $(BUILD_DIR)

.PHONY: verilog clean
