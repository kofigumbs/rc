ELM_MAKE_FLAGS ?= --optimize
export PATH := node_modules/bin:$(PATH)

.PHONY: all
all:
	elm make src/Cgol.elm  ${ELM_MAKE_FLAGS} --output build/animated-svg-cgol.html
	elm make src/Floss.elm ${ELM_MAKE_FLAGS} --output build/bathroom-floss.html
	elm make src/Warp.elm  ${ELM_MAKE_FLAGS} --output build/bitmoji-warp/elm.js
