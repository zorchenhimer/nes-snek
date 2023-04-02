
.include "nes2header.inc"
nes2mapper 0
nes2prg 1 * 16 * 1024
nes2chr 1 * 8 * 1024
;nes2wram 1 * 8 * 1024
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

BinOutput: .res 3

; Tile IDs for the next update
NextHead: .res 1
PrevHead: .res 1 ; new tile for the prev head
NextTail: .res 1
PrevTail: .res 1 ; background tile ID

NextHeadAddr: .res 2
PrevHeadAddr: .res 2
;NextTailAddr: .res 2
;PrevTailAddr: .res 2
TailAddr: .res 2

; Direction of travel w/o input
Direction: .res 1
NextDirection: .res 1
TailDirection: .res 1

Controller: .res 1
Controller_Old: .res 1

SPEED = 10
FAST = 7
Count: .res 1

Elongate: .res 1
Collided: .res 1

; address pointer into SpriteLookup
SpriteAddrPointer: .res 2
SpriteCoordPointer: .res 2
Countdown: .res 2

Survive: .res 1

.enum Dir
Up = 0
Right ; 01
Down  ; 10
Left  ; 11
.endenum

.segment "OAM"
SpriteZero: .res 4

LogoSprites: .res (8*3*4)

LogoXOffset = 96
LogoYOffset = 63

OtherSpriteCount = 35
OtherSprites: .res OtherSpriteCount*4
CountSprites: .res 3*4
Food: .res 4

.segment "BSS"

.segment "PLAYFIELD" ; starts at $400
Playfield: .res $400

.segment "VECTORS0"
    .word NMI
    .word RESET
    .word IRQ

.segment "CHR0"
    .incbin "snek.chr"
    .incbin "font.chr"
    .incbin "background-tiles.chr"

.segment "CHRHEX"
    .incbin "hex.chr"

SpriteId = $1E

.enum Corner
Right_UP   = 0
Up_Right   = 1
Right_Down = 2
Down_Right = 3

Down_Left = 0
Left_Up   = 1
Up_Left   = 2
Left_Down = 3
.endenum

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
PlayfieldA = $86
PlayfieldB = $87

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

    lda #$FF
    sta Sleeping

    lda #$00
    sta $2003
    lda #$02
    sta $4014

    lda #0
    sta $2005
    sta $2005

    lda #%1000_0000
    sta $2000

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

    ;lda #$20
    ;sta $2006
    ;sta TailAddr+1
    ;lda #$A3
    ;sta TailAddr+0
    ;sta $2006

    lda #$20
    sta NextHeadAddr+1
    sta TailAddr+1
    sta $2006

    lda #$A5
    sta NextHeadAddr+0
    lda #$A3
    sta TailAddr+0
    sta $2006

    lda #SNEK::Butt_Right ^ $10
    sta $2007
    lda #SNEK::Horizontal
    sta $2007
    lda #SNEK::Head_Right
    sta $2007

    lda #$04
    sta AddressPointer+1
    lda #$A3
    sta AddressPointer+0

    ldy #0
    lda #SNEK::Butt_Right
    sta (AddressPointer), y
    iny
    lda #SNEK::Horizontal
    sta (AddressPointer), y
    iny
    lda #SNEK::Head_Right
    sta (AddressPointer), y

TotalFood = (28*20)-3+1
    lda #.lobyte(TotalFood)
    sta Countdown+0
    lda #.hibyte(TotalFood)
    sta Countdown+1

    ;jsr DrawDebugSnek

    lda #Dir::Right
    sta TailDirection

    lda #Dir::Right
    sta Direction
    sta NextDirection

    jsr ResetSpritePointers

    ; Setup first pickup
    jsr NextSprite
    lda #SpriteId
    sta Food+1
    lda #1
    sta Food+2

    ; SNEK
    jsr SetLogo

WordsYOffset = 8*6
WordsXOffset = 8*1

    ; Press
    ldy #1
    .repeat 5, i
    lda #LogoXOffset+(i*8)+WordsXOffset
    sta OtherSprites+(i*4)+3
    lda #LogoYOffset+WordsYOffset
    sta OtherSprites+(i*4)+0
    lda MenuA+i
    sta OtherSprites+(i*4)+1
    sty OtherSprites+(i*4)+2
    .endrepeat

    ; Start
    .repeat 5, i
    lda #LogoXOffset+(i*8)+WordsXOffset+8
    sta OtherSprites+(i*4)+(5*4)+3
    lda #LogoYOffset+WordsYOffset+8
    sta OtherSprites+(i*4)+(5*4)+0
    lda MenuB+i
    sta OtherSprites+(i*4)+(5*4)+1
    sty OtherSprites+(i*4)+(5*4)+2
    .endrepeat

    jsr WritePausedPal

    sta $2005
    sta $2005

    lda #%1000_0000
    sta $2000

    lda #%0001_1110
    sta $2001

    jsr WaitForNMI
    jsr WaitForNMI

    lda #SPEED
    sta Count

StartFrame:

    jsr ReadControllers
    lda #BUTTON_START
    jsr ButtonPressed
    bne :+
    jsr WaitForNMI
    jmp StartFrame

:   jsr ClearLogo
    ldx #0
:
    sta OtherSprites+0, x
    inx
    inx
    inx
    inx
    cpx #(OtherSpriteCount*4)
    bne :-

    ; Initial Countdown values
    .repeat 3, i
    lda #208
    sta CountSprites+(4*i)+0
    lda BinOutput+i
    ora #$F0
    sta CountSprites+(4*i)+1
    lda #1
    sta CountSprites+(4*i)+2
    lda #(24 + (8*i))
    sta CountSprites+(4*i)+3
    .endrepeat

    jsr WaitForNMI
    jsr WriteGamePal
    lda #0
    sta $2005
    sta $2005
    lda #%1000_0000
    sta $2000

Frame:

    lda Survive
    beq :+

    sec
    lda Countdown+0
    sbc #1
    sta Countdown+0
    lda Countdown+1
    sbc #1
    sta Countdown+1
    bne :+
    lda Countdown+0
    bne :+

    ; game over, win
    jmp Collide

:

    .repeat 3, i
    lda BinOutput+i
    ora #$F0
    sta CountSprites+(4*i)+1
    .endrepeat

    ; Find next direction
    jsr ReadControllers
    lda #BUTTON_UP
    jsr ButtonPressed
    beq :+
    lda Direction
    cmp #Dir::Down
    beq :+
    lda #Dir::Up
    sta NextDirection
:
    lda #BUTTON_RIGHT
    jsr ButtonPressed
    beq :+
    lda Direction
    cmp #Dir::Left
    beq :+
    lda #Dir::Right
    sta NextDirection
:
    lda #BUTTON_DOWN
    jsr ButtonPressed
    beq :+
    lda Direction
    cmp #Dir::Up
    beq :+
    lda #Dir::Down
    sta NextDirection
:
    lda #BUTTON_LEFT
    jsr ButtonPressed
    beq :+
    lda Direction
    cmp #Dir::Right
    beq :+
    lda #Dir::Left
    sta NextDirection
:

    lda #BUTTON_A
    and Controller
    beq :+

    lda #1
    sta Elongate
:

    dec Count
    beq :+
    jsr WaitForNMI
    jmp Frame
:

    lda Survive
    bne :+
    lda #SPEED
    jmp :++
:   lda #FAST
:   sta Count

    lda NextDirection
    sta Direction

    ;jsr WaitForNMI

    ; 0 = normal mode
    ; 1 = survive, elongate once
    ; 2 = survive
    lda Survive
    beq :+
    cmp #1
    bne @notLonger
    lda #2
    sta Survive
    jmp @tailDone
:

    lda Elongate
    beq @notLonger
    lda #0
    sta Elongate
    jmp @tailDone

@notLonger:
    lda TailAddr+1
    sta $2006
    lda TailAddr+0
    sta $2006
    lda $2007
    lda $2007
    sta PrevTail
    and #$F0
    bne :+
    ; light tile
    ldx #PlayfieldB
    jmp :++
:
    ; dark tile
    ldx #PlayfieldA
:
    ; Write playfield tile
    lda TailAddr+1
    sta $2006
    lda TailAddr+0
    sta $2006
    stx $2007

    lda TailAddr+0
    sta AddressPointer+0
    sec
    lda TailAddr+1
    sbc #$1C
    sta AddressPointer+1
    lda #0
    tay
    sta (AddressPointer), y

    lda PrevTail
    and #$03
    sta PrevTail
    bne :+
    ; Up
    lda #32
    jmp @tailSub

:   cmp #1
    bne :+
    ; right
    lda #1
    jmp @tailAdd

:   cmp #2
    bne :+
    ; down
    lda #32
    jmp @tailAdd

:   ; left
    lda #1
@tailSub:
    sta TmpX
    sec
    lda TailAddr+0
    sbc TmpX
    sta TailAddr+0
    lda TailAddr+1
    sbc #0
    sta TailAddr+1
    jmp @tailMathDone

@tailAdd:
    clc
    adc TailAddr+0
    sta TailAddr+0
    lda #0
    adc TailAddr+1
    sta TailAddr+1

@tailMathDone:
    ; write the new tail in the correct direction
    lda TailAddr+1
    sta $2006
    lda TailAddr+0
    sta $2006
    lda $2007
    lda $2007
    sta NextTail
    and #$0F
    cmp #SNEK::Vertical
    beq :+
    cmp #SNEK::Horizontal
    bne @tailCorners

:   lda NextTail
    and #$F0
    ora PrevTail
    ora #%0000_00100

    ldx TailAddr+1
    stx $2006
    ldx TailAddr+0
    stx $2006
    sta $2007
    jmp @tailDone

@tailCorners:
    ; tail tile in A, AND'd with $0F
    cmp #SNEK::Corner_TR
    bne :++
    lda PrevTail
    cmp #Dir::Right
    bne :+
    ; right -> down
    ldx #SNEK::Butt_Down
    jmp @tailWrite

    ; up -> left
:   ldx #SNEK::Butt_Left
    jmp @tailWrite

:   cmp #SNEK::Corner_TL
    bne :++
    lda PrevTail
    cmp #Dir::Left
    bne :+
    ; left -> down
    ldx #SNEK::Butt_Down
    jmp @tailWrite

    ; up -> right
:   ldx #SNEK::Butt_Right
    jmp @tailWrite

:   cmp #SNEK::Corner_BR
    bne :++
    lda PrevTail
    cmp #Dir::Right
    bne :+
    ; right -> up
    ldx #SNEK::Butt_Up
    jmp @tailWrite

:   ; down -> left
    ldx #SNEK::Butt_Left
    jmp @tailWrite

:   ;bottom left corner
    lda PrevTail
    cmp #Dir::Left
    bne :+
    ; left -> up
    ldx #SNEK::Butt_Up
    jmp @tailWrite

:   ; down -> right
    ldx #SNEK::Butt_Right

@tailWrite:
    stx PrevTail
    lda NextTail
    and #$F0
    ora PrevTail
    tax
    lda TailAddr+1
    sta $2006
    lda TailAddr+0
    sta $2006
    stx $2007

@tailDone:

; Move Head
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

    lda NextHeadAddr+0
    sta AddressPointer+0
    lda NextHeadAddr+1
    sec
    sbc #$1C
    sta AddressPointer+1
    ldy #0
    lda (AddressPointer), y
    bpl :+
    ; found an item
    lda #1
    sta Elongate

    ; Look at next tile, make sure it isn't a snek
:   bit $2002
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
    ldy #1
    sty Collided
    and #$F0

@tileA:
    lda #$10
    jmp @update

@tileB:
    lda #0

@update:
    ; no collide, write tiles
    ; New head
    ora NextHead
    ldx NextHeadAddr+1
    stx $2006
    ldx NextHeadAddr+0
    stx $2006
    sta $2007

    ; Add head to ram
    lda NextHeadAddr+0
    sta AddressPointer+0
    sec
    lda NextHeadAddr+1
    sbc #$1C    ; get in the proper range for ram (starts at $400)
    sta AddressPointer+1
    lda #1
    ldy #0
    sta (AddressPointer), y

    ; remove old head
    ldx PrevHeadAddr+1
    stx $2006
    ldx PrevHeadAddr+0
    stx $2006
    lda $2007
    lda $2007
    sta PrevHead
    and #$03
    cmp Direction
    beq @straight

    cmp #Dir::Up
    bne @checkRight
    lda Direction
    cmp #Dir::Right
    bne :+
    ; up -> right
    lda #SNEK::Corner_TL
    jmp @cornerCheckDone
:   ; up -> left
    lda #SNEK::Corner_TR
    jmp @cornerCheckDone

@checkRight:
    cmp #Dir::Right
    bne @checkDown
    lda Direction
    cmp #Dir::Up
    bne :+
    ; right -> down
    lda #SNEK::Corner_BR
    jmp @cornerCheckDone
:   ; right -> up
    lda #SNEK::Corner_TR
    jmp @cornerCheckDone

@checkDown:
    cmp #Dir::Down
    bne @checkLeft
    lda Direction
    cmp #Dir::Right
    bne :+
    ; down -> right
    lda #SNEK::Corner_BL
    jmp @cornerCheckDone
:   ; down -> left
    lda #SNEK::Corner_BR
    jmp @cornerCheckDone

@checkLeft:
    lda Direction
    cmp #Dir::Up
    bne :+
    ; left -> down
    lda #SNEK::Corner_BL
    jmp @cornerCheckDone
:   ; left -> up
    lda #SNEK::Corner_TL

@cornerCheckDone:
    sta TmpX
    lda PrevHead
    and #$F0
    ora TmpX

    ldx PrevHeadAddr+1
    stx $2006
    ldx PrevHeadAddr+0
    stx $2006
    sta $2007
    jmp @headDone

@straight:
    ; need the A/B color for the background
    and #1
    ora #SNEK::Vertical
    sta TmpX
    lda PrevHead
    and #$F0
    ora TmpX

    ldx PrevHeadAddr+1
    stx $2006
    ldx PrevHeadAddr+0
    stx $2006
    sta $2007

@headDone:
    lda Collided
    beq :+
    jmp Collide
:

    lda #0
    sta $2005
    sta $2005

    lda Elongate
    beq :+
    jsr NextSprite
:   jmp Frame

Collide:
    lda #%1000_0000
    sta $2000

    lda #0
    sta $2005
    sta $2005

    jsr SetLogo

    ; Game
    ldy #1
    .repeat 4, i
    lda #LogoXOffset+(i*8)+WordsXOffset
    sta OtherSprites+(i*4)+3
    lda #LogoYOffset+WordsYOffset
    sta OtherSprites+(i*4)+0
    lda DedA+i
    sta OtherSprites+(i*4)+1
    sty OtherSprites+(i*4)+2
    .endrepeat

    ; Over
    .repeat 4, i
    lda #LogoXOffset+(i*8)+WordsXOffset+16
    sta OtherSprites+(i*4)+(5*4)+3
    lda #LogoYOffset+WordsYOffset+8
    sta OtherSprites+(i*4)+(5*4)+0
    lda DedB+i
    sta OtherSprites+(i*4)+(5*4)+1
    sty OtherSprites+(i*4)+(5*4)+2
    .endrepeat

    jsr WaitForNMI
    jsr WritePausedPal

    lda #%1000_0000
    sta $2000

    lda #0
    sta $2005
    sta $2005

DedFrame:

    jsr ReadControllers
    lda #BUTTON_START
    jsr ButtonPressed
    bne :+
    jsr WaitForNMI
    jmp DedFrame

:   jsr WaitForNMI
    lda #0
    sta $2000
    jmp RESET

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

ResetSpritePointers:
    lda #.lobyte(SpriteLookup)
    sta SpriteAddrPointer+0
    lda #.hibyte(SpriteLookup)
    sta SpriteAddrPointer+1

    lda #.lobyte(SpriteCoords)
    sta SpriteCoordPointer+0
    lda #.hibyte(SpriteCoords)
    sta SpriteCoordPointer+1
    rts

IncSpritePointers:
    clc
    lda SpriteAddrPointer+0
    adc #2
    sta SpriteAddrPointer+0
    lda SpriteAddrPointer+1
    adc #0
    sta SpriteAddrPointer+1
    ldy #1
    lda (SpriteAddrPointer), y
    bne :+
    jmp ResetSpritePointers

:   clc
    lda SpriteCoordPointer+0
    adc #2
    sta SpriteCoordPointer+0
    lda SpriteCoordPointer+1
    adc #0
    sta SpriteCoordPointer+1
    rts

; Draw a new sprite and add it to RAM
NextSprite:
    lda Survive
    beq @next
    jmp BinToDec
    ;rts

@next:
    ldy #0
    lda (SpriteAddrPointer), y
    sta AddressPointer+0
    iny
    lda (SpriteAddrPointer), y
    sta AddressPointer+1
    dey
    ldy #0
    lda (AddressPointer), y
    beq :+
    jsr IncSpritePointers
    jmp @next
:

    lda #$80
    sta (AddressPointer), y
    lda (SpriteCoordPointer), y
    sta Food+0
    iny
    lda (SpriteCoordPointer), y
    sta Food+3

    lda Countdown+0
    sec
    sbc #1
    sta Countdown+0
    lda Countdown+1
    sbc #0
    sta Countdown+1

    ;cmp #$FF
    lda Countdown+1
    bne :+
    lda Countdown+0
    bne :+
    ;;
    ;; No more spaces left; SURVIVE
    ;;
    jmp SetSurvive

:   jsr IncSpritePointers
    lda Countdown+0
    sta TmpX
    lda Countdown+1
    sta TmpY
    jmp BinToDec
    ;rts

WritePausedPal:
    lda #$3F
    sta $2006
    lda #$00
    sta $2006

    ldx #0
:
    lda PausedPalette_BG, x
    sta $2007
    inx
    cpx #32
    bne :-
    rts

WriteGamePal:
    lda #$3F
    sta $2006
    lda #$00
    sta $2006

    ldx #0
:
    lda GamePalette_BG, x
    sta $2007
    inx
    cpx #32
    bne :-
    rts

ClearLogo:
    lda #$FE
    ldx #0
:
    sta LogoSprites+0, x
    inx
    inx
    inx
    inx

    cpx #(8*3*4)
    bne :-
    rts

SetLogo:
    ldx #0 ; offset
    ldy #0 ; index
:
    lda LogoIds, y
    sta LogoSprites+1, x
    clc
    lda LogoX, y
    adc #LogoXOffset
    sta LogoSprites+3, x
    lda LogoY, y
    adc #LogoYOffset
    sta LogoSprites+0, x
    lda #0
    sta LogoSprites+2, x

    inx
    inx
    inx
    inx

    iny
    cpy #(8*3)
    bne :-
    rts

; Binary value in A, decimal values
; output in BinOutput
BinToDec:
    lda #0
    sta BinOutput+0
    sta BinOutput+1
    sta BinOutput+2
@high:
    lda TmpY
    beq @hundo

    sec
    lda TmpX
    sbc #100
    sta TmpX
    inc BinOutput+0
    bcs @high
    dec TmpY
    jmp @high

@hundo:
    lda TmpX
    cmp #100
    bcs @addhundo
    jmp @tens

@addhundo:
    inc BinOutput+0
    sec
    sbc #100
    sta TmpX
    jmp @hundo

@tens:
    lda TmpX
    cmp #10
    bcs @addtens
    jmp @done

@addtens:
    inc BinOutput+1
    sec
    sbc #10
    sta TmpX
    jmp @tens

@done:
    lda TmpX
    sta BinOutput+2
    rts

DrawDebugSnek:
    ; Top row, horiz with head & tail
    lda #$20
    sta $2006
    lda #$82
    sta $2006

    lda #SNEK::Corner_TL
    sta $2007
    lda #SNEK::Horizontal
    sta $2007
    sta $2007
    sta $2007
    lda #SNEK::Head_Right
    sta $2007
    lda #$86
    sta $2007
    lda #$87
    sta $2007
    lda #$86
    sta $2007
    lda #SNEK::Butt_Right
    sta $2007

    lda #SNEK::Horizontal
    .repeat 18
    sta $2007
    .endrepeat

    lda #SNEK::Corner_TR
    sta $2007

    lda #$20
    sta $2006
    lda #$A2
    sta $2006

    lda #SNEK::Vertical
    sta $2007

    lda #SNEK::Corner_TL
    ldy #SNEK::Corner_TR
    .repeat 13
    sta $2007
    sty $2007
    .endrepeat
    ldy #SNEK::Vertical
    sty $2007

    .repeat 17, i
    lda #.hibyte($20C2 + (i*32))
    sta $2006
    lda #.lobyte($20C2 + (i*32))
    sta $2006
    .repeat 28
    sty $2007
    .endrepeat
    .endrepeat

    lda #$22
    sta $2006
    lda #$E2
    sta $2006

    lda #SNEK::Corner_BL
    ldy #SNEK::Corner_BR
    .repeat 14
    sta $2007
    sty $2007
    .endrepeat

    lda #$20
    sta TailAddr+1
    sta NextHeadAddr+1
    lda #$8A
    sta TailAddr+0
    lda #$86
    sta NextHeadAddr+0

    ldy #1
    .repeat 5, i
    sty $0482+i
    .endrepeat

    .repeat 630, i
    sty $0489+i
    .endrepeat

    lda #0
    sta Countdown+1
    lda #3+1
    sta Countdown+0
    rts

SetSurvive:
    lda #1
    sta Survive
    lda #60
    sta Countdown+0
    lda #0
    sta Countdown+1
    lda #FAST
    sta Count
    lda #$FE
    sta Food+0
    rts

GamePalette_BG:
    .byte $19, $21, $11, $01
    .byte $19, $09, $17, $27
    .byte $19, $19, $0C, $1C
    .byte $19, $0F, $0F, $31

GamePalette_SP:
    .byte $19, $20, $17, $27
    .byte $19, $20, $10, $35
    .byte $19, $15, $25, $35
    .byte $19, $15, $25, $35

PausedPalette_BG:
    .byte $00, $00, $00, $2D
    .byte $00, $2D, $10, $3D
    .byte $00, $0F, $0F, $0F
    .byte $00, $0F, $0F, $0F

PausedPalette_SP:
    .byte $00, $10, $17, $27
    .byte $00, $20, $10, $0F
    .byte $00, $0F, $0F, $0F
    .byte $00, $0F, $0F, $0F

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

    .repeat (5*8)
    .word 0
    .endrepeat
SpriteLookup:
    .include "rand.i"
    .word 0

SpriteCoords:
    .include "coords.i"
    .word 0

LogoIds:
    .byte $03, $09, $03, $02, $03, $07, $06, $08
    .byte $01, $02, $0C, $0C, $0C, $07, $0C, $0E
    .byte $05, $00, $04, $0A, $01, $09, $0A, $04

; offset coordinates
LogoX:
    .repeat 3
        .repeat 8, i
            .byte i*8
        .endrepeat
    .endrepeat

LogoY:
    .repeat 3, i
        .repeat 8
            .byte i*8
        .endrepeat
    .endrepeat

MenuA: .byte "Press"
MenuB: .byte "Start"

DedA: .byte "Game"
DedB: .byte "Over"
