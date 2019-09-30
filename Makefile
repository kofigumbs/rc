BUILD        = $(CURDIR)/.build
export PATH := node_modules/bin:$(PATH)

.PHONY: all
all: ${BUILD}/animated-svg-cgol.html

${BUILD}/animated-svg-cgol.html: animated-svg-cgol/*.elm
	cd animated-svg-cgol && elm make Main.elm --output $@