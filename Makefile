BUILD        = $(CURDIR)/.build
export PATH := node_modules/bin:$(PATH)

.PHONY: all
all: ${BUILD} ${BUILD}/animated-svg-cgol.html
${BUILD}:
	mkdir ${BUILD}

${BUILD}/animated-svg-cgol.html: ${BUILD} animated-svg-cgol/*.elm
	cd animated-svg-cgol && elm make Main.elm --output $@
