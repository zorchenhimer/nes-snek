
.include "nes2header.inc"
nes2mapper 0
nes2prg 1 * 16 * 1024
nes2chr 1 * 8 * 1024
nes2mirror 'V'
nes2tv 'N'
nes2end

.feature leading_dot_in_identifiers
.feature underline_in_numbers
.feature addrsize

.segment "ZEROPAGE"
Sleeping: .res 1
AddressPointer: .res 2
AddressPointer2: .res 2

TmpX: .res 1
TmpY: .res 1
TmpZ: .res 1

; Tile IDs for the next update
NextHead: .res 1
PrevHead: .res 1 ; new tile for the prev head
NextTail: .res 1
PrevTail: .res 1 ; background tile ID

NextHeadAddr: .res 2
PrevHeadAddr: .res 2
NextTailAddr: .res 2
PrevTailAddr: .res 2

; Direction of travel w/o input
Direction: .res 1
NextDirection: .res 1

Controller: .res 1
Controller_Old: .res 1

SPEED = 10
Count: .res 1

.enum Dir
Up = 0
Right ; 01
Down  ; 10
Left  ; 11
.endenum

.segment "OAM"
SpriteZero: .res 4
Sprites: .res (4 * 63)

.segment "BSS"

.segment "VECTORS0"
    .word NMI
    .word RESET
    .word IRQ

.segment "CHR0"
    ;.incbin "pattern-a.chr"
@bgTiles:
    .incbin "snek.chr"
    .incbin "background-tiles.chr"

.enum SNEK
Corner_BR = 0
Corner_BL
Corner_TR
Corner_TL
Butt_Up
Butt_Right
Butt_Down
Butt_Left
Head_Up
Head_Right
Head_Down
Head_Left

Vertical    ; 0C = 0
Horizontal  ; 0D = 1
.endenum

; Tiles for the playfield
PlayfieldA = $26
PlayfieldB = $27

.segment "CHR1"
    ;.incbin "pattern-a.chr"

.segment "PAGE0"

; Button Constants
BUTTON_A        = 1 << 7
BUTTON_B        = 1 << 6
BUTTON_SELECT   = 1 << 5
BUTTON_START    = 1 << 4
BUTTON_UP       = 1 << 3
BUTTON_DOWN     = 1 << 2
BUTTON_LEFT     = 1 << 1
BUTTON_RIGHT    = 1 << 0

IRQ:
    rti

NMI:
    pha
    ;txa
    ;pha
    ;tya
    ;pha

    lda #$FF
    sta Sleeping

    lda #$00
    sta $2003
    lda #$02
    sta $4014

    lda #0
    sta $2005
    sta $2005

    ;pla
    ;tay
    ;pla
    ;tax
    pla
    rti

RESET:
    sei         ; Disable IRQs
    cld         ; Disable decimal mode

    ldx #$40
    stx $4017   ; Disable APU frame IRQ

    ldx #$FF
    txs         ; Setup new stack

    inx         ; Now X = 0

    stx $2000   ; disable NMI
    stx $2001   ; disable rendering
    stx $4010   ; disable DMC IRQs

:   ; First wait for VBlank to make sure PPU is ready.
    bit $2002   ; test this bit with ACC
    bpl :- ; Branch on result plus

:   ; Clear RAM
    lda #$00
    sta $0000, x
    sta $0100, x
    sta $0200, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x

    inx
    bne :-  ; loop if != 0

:   ; Second wait for vblank.  PPU is ready after this
    bit $2002
    bpl :-

    ; Clear sprites
    ldx #0
    lda #$FF
:
    sta $200, x
    inx
    bne :-

    lda #$00
    sta $2003
    lda #$02
    sta $4014

    ; Write initial palette values
    lda #$3F
    sta $2006
    lda #$00
    sta $2006
    ldx #0
:
    lda Palette_BG, x
    sta $2007
    inx
    cpx #16
    bne :-

    ldx #0
:
    lda Palette_SP, x
    sta $2007
    inx
    cpx #16
    bne :-

    lda #$23
    sta $2006
    lda #$C0
    sta $2006

    ; Set the attr table to something
    ldy #64
    lda #$FF
:
    sta $2007
    dey
    bne :-

    jsr DrawPlayfield

    lda #$20
    sta $2006
    sta PrevTailAddr+1

    lda #$A3
    sta PrevTailAddr+0
    sta $2006

    lda #$20
    sta NextHeadAddr+1
    sta NextTailAddr+1
    lda #$A5
    sta NextHeadAddr+0
    lda #$A4
    sta NextTailAddr+0

    lda #SNEK::Butt_Right ^ $10
    sta $2007
    lda #SNEK::Horizontal
    sta $2007
    lda #SNEK::Head_Right
    sta $2007

    lda #Dir::Right
    sta Direction
    sta NextDirection

    lda #0
    sta $2005
    sta $2005

    lda #$88
    sta $2000

    lda #%0001_1110
    sta $2001

    jsr WaitForNMI
    jsr WaitForNMI

    lda #SPEED
    sta Count

Frame:
    ; Find next direction
    jsr ReadControllers
    lda #BUTTON_UP
    and Controller
    beq :+
    lda Direction
    cmp #Dir::Down
    beq :+
    lda #Dir::Up
    sta NextDirection
:
    lda #BUTTON_RIGHT
    and Controller
    beq :+
    lda Direction
    cmp #Dir::Left
    beq :+
    lda #Dir::Right
    sta NextDirection
:
    lda #BUTTON_DOWN
    and Controller
    beq :+
    lda Direction
    cmp #Dir::Up
    beq :+
    lda #Dir::Down
    sta NextDirection
:
    lda #BUTTON_LEFT
    and Controller
    beq :+
    lda Direction
    cmp #Dir::Right
    beq :+
    lda #Dir::Left
    sta NextDirection
:

    dec Count
    beq :+
    jsr WaitForNMI
    jmp Frame

:   lda #SPEED
    sta Count

    lda NextDirection
    sta Direction

    lda NextHeadAddr+0
    sta PrevHeadAddr+0
    lda NextHeadAddr+1
    sta PrevHeadAddr+1

    lda Direction
    and #$03
    cmp #Dir::Up
    bne :+
    lda #SNEK::Head_Up
    sta NextHead
    lda #32
    jmp @dirSub

:   cmp #Dir::Right
    bne :+
    lda #SNEK::Head_Right
    sta NextHead
    lda #1
    jmp @dirAdd

:   cmp #Dir::Down
    bne :+
    lda #SNEK::Head_Down
    sta NextHead
    lda #32
    jmp @dirAdd

:   lda #SNEK::Head_Left
    sta NextHead
    lda #1

@dirSub:
    sta TmpX
    sec
    lda NextHeadAddr+0
    sbc TmpX
    sta NextHeadAddr+0
    lda NextHeadAddr+1
    sbc #0
    sta NextHeadAddr+1
    jmp @dirDone

@dirAdd:
    clc
    adc NextHeadAddr+0
    sta NextHeadAddr+0
    lda #0
    adc NextHeadAddr+1
    sta NextHeadAddr+1

@dirDone:
    jsr WaitForNMI

    ; Look at next tile, make sure it isn't a snek
    bit $2002
    lda NextHeadAddr+1
    sta $2006
    lda NextHeadAddr+0
    sta $2006
    lda $2007
    lda $2007
    cmp #PlayfieldA
    beq @tileA
    cmp #PlayfieldB
    beq @tileB
    jmp @collide

@tileA:
    lda #$10
    jmp @update

@tileB:
    lda #0

@update:
    ; no collide, write tiles
    ; New head first
    ora NextHead
    ldx NextHeadAddr+1
    stx $2006
    ldx NextHeadAddr+0
    stx $2006
    sta $2007

    ; remove old head
    ldx PrevHeadAddr+1
    stx $2006
    ldx PrevHeadAddr+0
    stx $2006
    lda $2007
    lda $2007
    and #$03
    sta PrevHead
    cmp Direction
    beq @straight
    ; TODO: figure out corners


@straight:
    and #1
    ora #SNEK::Vertical
    ldx PrevHeadAddr+1
    stx $2006
    ldx PrevHeadAddr+0
    stx $2006
    sta $2007

    lda #0
    sta $2005
    sta $2005

    jmp Frame

@collide:
    ;;
    ;; TODO: Collide
    ;;
    brk

WaitForNMI:
:   bit Sleeping
    bpl :-
    lda #0
    sta Sleeping
    rts

DrawPlayfield:
    lda #.hibyte(PlayfieldData)
    sta AddressPointer+1
    lda #.lobyte(PlayfieldData)
    sta AddressPointer+0

    bit $2002
    lda #$20
    sta $2006
    lda #$00
    sta $2006

    lda #0
    sta TmpX
@metaRows:
    ldy #0
    sty TmpY
@firstRow:
    ;; lookup meta id in playfield
    lda (AddressPointer), y
    asl a
    tax
    lda MetatileData+0, x
    sta AddressPointer2+0
    lda MetatileData+1, x
    sta AddressPointer2+1

    ldy #4
    lda (AddressPointer2), y
    ;lda MetaTileIDs, x
    sta $2007
    iny
    lda (AddressPointer2), y
    ;lda MetaTileIDs+1, x
    sta $2007

    inc TmpY
    ldy TmpY
    cpy #16
    bne @firstRow

    ldy #0
    sty TmpY
@secondRow:
    lda (AddressPointer), y
    asl a
    tax
    lda MetatileData+0, x
    sta AddressPointer2+0
    lda MetatileData+1, x
    sta AddressPointer2+1

    ldy #6
    lda (AddressPointer2), y
    ;lda MetaTileIDs+2, x
    sta $2007
    iny
    lda (AddressPointer2), y
    ;lda MetaTileIDs+3, x
    sta $2007

    inc TmpY
    ldy TmpY
    cpy #16
    bne @secondRow

    lda #16
    clc
    adc AddressPointer+0
    sta AddressPointer+0
    lda #0
    adc AddressPointer+1
    sta AddressPointer+1

    inc TmpX
    lda TmpX
    cmp #15
    bne @metaRows

    lda #.hibyte(PlayfieldData)
    sta AddressPointer+1
    lda #.lobyte(PlayfieldData)
    sta AddressPointer+0

    lda #$23
    sta $2006
    lda #$C0
    sta $2006

    ldy #0
    sty TmpX
@loopAttr:
    lda #0
    sta TmpY ; tmp attr value

    lda (AddressPointer), y
    jsr GetMetaPalette

    sta TmpY
    iny

    lda (AddressPointer), y
    jsr GetMetaPalette

    asl a
    asl a
    ora TmpY
    sta TmpY
    dey
    tya
    clc
    adc #16
    tay

    lda (AddressPointer), y
    jsr GetMetaPalette

    asl a
    asl a
    asl a
    asl a
    ora TmpY
    sta TmpY
    iny

    lda (AddressPointer), y
    jsr GetMetaPalette

    asl a
    asl a
    asl a
    asl a
    asl a
    asl a
    ora TmpY
    sta $2007

    tya
    sec
    sbc #15
    tay

    cpy #16
    bne @loopAttr

    clc
    lda #32
    adc AddressPointer+0
    sta AddressPointer+0
    lda #0
    adc AddressPointer+1
    sta AddressPointer+1

    ldy #0

    inc TmpX
    lda TmpX
    cmp #8
    bne @loopAttr
    rts

; Expects metatile ID in A
; Returns palette value in A
GetMetaPalette:
    asl a
    tax

    lda MetatileData+0, x
    sta AddressPointer2+0
    lda MetatileData+1, x
    sta AddressPointer2+1

    sty TmpZ
    ldy #2
    lda (AddressPointer2), y
    ldy TmpZ
    rts

Palette_BG:
    .byte $19, $21, $11, $01
    .byte $19, $09, $17, $27
    .byte $19, $19, $0C, $1C
    .byte $19, $0F, $0F, $31

Palette_SP:
    .byte $19, $15, $25, $35
    .byte $19, $15, $25, $35
    .byte $19, $15, $25, $35
    .byte $19, $15, $25, $35

; Lookup tables for the playfield.  Address
; is the start of the PPU row.
PlayfieldAddrHi:
    .repeat 20, i
        .byte .hibyte($2082+(i*32))
    .endrepeat

PlayfieldAddrLo:
    .repeat 20, i
        .byte .lobyte($2082+(i*32))
    .endrepeat

PlayfieldData:
    .include "playfield.i"

MetatileData:
    .include "background-tiles.i"

    .include "utils.asm"
