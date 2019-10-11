export PATH := node_modules/bin:$(PATH)

.PHONY: all
all:
	elm make Cgol.elm    --optimize --output .build/animated-svg-cgol.html
	elm make Floss.elm   --optimize --output .build/bathroom-floss.html
	elm make Bitmoji.elm --optimize --output .build/bitmoji-warp.html
