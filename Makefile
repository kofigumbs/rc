ELM_MAKE_FLAGS ?= --optimize
export PATH := ${PWD}/bin:${PATH}

.PHONY: all elm
all: bin/elm elm

bin/elm:
	# https://github.com/elm/compiler/tree/master/installers/linux
	curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
	gunzip elm.gz
	chmod +x elm
	mkdir -p bin
	mv elm bin

elm:
	elm make src/Cgol.elm        ${ELM_MAKE_FLAGS} --output build/animated-svg-cgol.html
	elm make src/Floss.elm       ${ELM_MAKE_FLAGS} --output build/bathroom-floss.html
	elm make src/Warp.elm        ${ELM_MAKE_FLAGS} --output build/bitmoji-warp/elm.js
	elm make src/Visualizer.elm  ${ELM_MAKE_FLAGS} --output build/visualizer/elm.js
