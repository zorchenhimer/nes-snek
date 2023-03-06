.PHONY: all env clean chr

EXT=
ifeq ($(OS),Windows_NT)
EXT=.exe
endif

CHRUTIL = go-nes/bin/chrutil$(EXT)
METATILES = go-nes/bin/metatiles$(EXT)
#CA = cc65/bin/ca65$(EXT)
#LD = cc65/bin/ld65$(EXT)

NAME = snake
NESCFG = nes_nrom.cfg
CAFLAGS = -g -t nes
LDFLAGS = -C $(NESCFG) --dbgfile bin/$(NAME).dbg -m bin/$(NAME).map

SOURCES = \
	main.asm \
	playfield.i \
	background-tiles.i

CHR = background-tiles.chr snek.chr

all: env chr bin/snake.nes
env: $(METATILES) $(CHRUTIL) bin/

clean:
	-rm bin/* *.chr *.i

cleanall: clean
	-rm images/*.bmp
	-$(MAKE) -C go-nes/ clean
#	-$(MAKE) -C cc65/ clean

bin/:
	-mkdir bin

bin/snake.nes: bin/main.o
	ld65 $(LDFLAGS) -o $@ $^

bin/main.o: $(SOURCES) $(CHR)
	ca65 $(CAFLAGS) -o $@ main.asm

#%.chr: images/%.bmp
#	$(CHRUTIL) $< -o $@ --remove-duplicates

#background-tiles.chr: images/background-tiles.bmp
#	$(CHRUTIL) $< -o $@ --remove-duplicates --pad-tiles 16

snek.chr: images/snek.bmp
	$(CHRUTIL) $< -o $@ --pad-tiles 32

images/snek.bmp: images/snek.aseprite
	aseprite -b $< \
		--crop 0,0,128,16 \
		--save-as $@

images/%.bmp: images/%.aseprite
	aseprite -b $< --save-as $@

playfield.i: layouts/playfield.tmx
	cd layouts && go run ../convert-map.go playfield.tmx ../playfield.i

background-tiles.chr background-tiles.i: images/background-tiles.bmp
	$(METATILES) $< $(basename $@).chr $(basename $@).i --offset 32

$(CHRUTIL):
	$(MAKE) -C go-nes/ bin/chrutil$(EXT)

$(METATILES):
	$(MAKE) -C go-nes/ bin/metatiles$(EXT)

