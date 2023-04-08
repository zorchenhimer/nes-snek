.PHONY: all env clean chr

EXT=
ifeq ($(OS),Windows_NT)
EXT=.exe
endif

CHRUTIL = go-nes/bin/chrutil$(EXT)
METATILES = go-nes/bin/metatiles$(EXT)

NAME = snek
NESCFG = nes_nrom.cfg
CAFLAGS = -g -t nes
LDFLAGS = -C $(NESCFG) --dbgfile bin/$(NAME).dbg -m bin/$(NAME).map

SOURCES = \
	main.asm \
	playfield.i \
	background-tiles.i \
	rand.inc coords.inc

CHR = background-tiles.chr snek.chr font.chr logo.chr hex.chr

all: env chr bin/snek.nes
env: $(METATILES) $(CHRUTIL) bin/

clean:
	-rm bin/* *.chr *.i

cleanall: clean
	-rm images/*.bmp *.inc
	-$(MAKE) -C go-nes/ clean

bin/:
	-mkdir bin

bin/snek.nes: bin/main.o
	ld65 $(LDFLAGS) -o $@ $^

bin/main.o: $(SOURCES) $(CHR)
	ca65 $(CAFLAGS) -o $@ main.asm

snek.chr: images/snek.bmp
	$(CHRUTIL) $< -o $@ --pad-tiles 32 --tile-count 32

font.chr: images/font.bmp
	$(CHRUTIL) $< -o $@ --tile-offset 32 --tile-count 96

logo.chr: images/snek.bmp
	$(CHRUTIL) $< -o $@ --tile-offset 32 --remove-empty

hex.chr: images/font.bmp
	$(CHRUTIL) $< -o $@ --tile-offset 240 --tile-count 16

images/snek.bmp: images/snek.aseprite
	aseprite -b $< \
		--save-as $@

images/%.bmp: images/%.aseprite
	aseprite -b $< --save-as $@

playfield.i: layouts/playfield.tmx
	cd layouts && go run ../convert-map.go playfield.tmx ../playfield.i

rand.inc corods.inc: rand.go
	go run $<

background-tiles.chr background-tiles.i: images/background-tiles.bmp
	$(METATILES) $< $(basename $@).chr $(basename $@).i --offset 128 --pad 16

$(CHRUTIL):
	$(MAKE) -C go-nes/ bin/chrutil$(EXT)

$(METATILES):
	$(MAKE) -C go-nes/ bin/metatiles$(EXT)

