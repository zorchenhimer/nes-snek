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
	background-tiles.i \
	rand.i

CHR = background-tiles.chr snek.chr font.chr logo.chr hex.chr

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
		#--crop 0,0,128,16 \

images/%.bmp: images/%.aseprite
	aseprite -b $< --save-as $@

playfield.i: layouts/playfield.tmx
	cd layouts && go run ../convert-map.go playfield.tmx ../playfield.i

rand.i: rand.go
	go run $<

background-tiles.chr background-tiles.i: images/background-tiles.bmp
	$(METATILES) $< $(basename $@).chr $(basename $@).i --offset 128 --pad 16

$(CHRUTIL):
	$(MAKE) -C go-nes/ bin/chrutil$(EXT)

$(METATILES):
	$(MAKE) -C go-nes/ bin/metatiles$(EXT)

