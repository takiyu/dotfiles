.PHONY: default
default:
	test -d build || make init
	make build

.PHONY: init
init:
	mkdir build
	cd build; cmake .. -DCMAKE_GENERATOR_PLATFORM=x64

.PHONY: build
build:
	cd build && cmake --build . --config release

.PHONY: rebuild
rebuild:
	cd build && cmake --build . --config release --clean-first

.PHONY: clean
clean:
	rm build -r
