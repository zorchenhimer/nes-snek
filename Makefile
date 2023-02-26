.PHONY: all env clean chr

EXT=
ifeq ($(OS),Windows_NT)
EXT=.exe
endif

CHRUTIL = go-nes/bin/chrutil$(EXT)
CA = cc65/bin/ca65$(EXT)
LD = cc65/bin/ld65$(EXT)

NAME = snake
NESCFG = nes_nrom.cfg
CAFLAGS = -g -t nes
LDFLAGS = -C $(NESCFG) --dbgfile bin/$(NAME).dbg -m bin/$(NAME).map

SOURCES = \
	main.asm

all: env chr bin/snake.nes
env: $(CA) $(LD) $(CHRUTIL) bin/
chr: pattern-a.chr

clean:
	-rm bin/* *.chr

cleanall: clean
	-$(MAKE) -C go-nes/ clean
	-$(MAKE) -C cc65/ clean

bin/:
	-mkdir bin

bin/snake.nes: bin/main.o
	$(LD) $(LDFLAGS) -o $@ $^

bin/main.o: $(SOURCES) $(CHR)
	$(CA) $(CAFLAGS) -o $@ main.asm

%.chr: images/%.bmp
	$(CHRUTIL) $< -o $@

%.bmp: images/%.aseprite
	aseprite -b $< --save-as $@

$(CHRUTIL):
	$(MAKE) -C go-nes/ bin/chrutil$(EXT)

$(CA):
	$(MAKE) -C cc65/ ca65

$(LD):
	$(MAKE) -C cc65/ ld65
