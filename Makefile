BUILD_DIR = ./build

verilog:
	@mkdir -p $(BUILD_DIR)
	sbt "run -td $(BUILD_DIR)"

clean:
	-rm -rf $(BUILD_DIR)

.PHONY: verilog clean
