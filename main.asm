
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
AddressPointer3: .res 2

TmpA: .res 1
TmpX: .res 1
TmpY: .res 1
TmpZ: .res 1

BinOutput: .res 3

; Tile IDs for the next update
NextHead: .res 1
PrevHead: .res 1 ; new tile for the prev head
NextTail: .res 1
PrevTail: .res 1 ; background tile ID

;NextHeadAddr: .res 2
;PrevHeadAddr: .res 2
;NextTailAddr: .res 2
;PrevTailAddr: .res 2
;TailAddr: .res 2

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
IsPaused: .res 1

PPUBuffer: .res 10*3
BufferIdx: .res 1

SnekHeadX: .res 1
SnekHeadY: .res 1

SnekTailX: .res 1
SnekTailY: .res 1

PrevX: .res 1
PrevY: .res 1

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
FoodSprite: .res 4

.segment "BSS"

.segment "PLAYFIELD" ; starts at $400
Playfield: .res 20*28

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
PlayfieldA = $86 ; dark
PlayfieldB = $87 ; light

Playfield_Width = 28
Playfield_Height = 20

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
    txa
    pha
    tya
    pha

    lda #$FF
    sta Sleeping

    lda #$00
    sta $2003
    lda #$02
    sta $4014

    lda IsPaused
    beq @notPaused
    jsr WritePausedPal
    jmp @pauseDone

@notPaused:
    jsr WriteGamePal

@pauseDone:

    lda BufferIdx
    bmi @bufferDone
    ldx #0
@loop:
    lda PPUBuffer, x
    sta TmpX
    inx

    ldy PPUBuffer, x
    inx

    clc
    lda PlayfieldAddrLo, y
    adc TmpX
    sta AddressPointer3+0

    lda PlayfieldAddrHi, y
    adc #0
    sta AddressPointer3+1

    sta $2006
    lda AddressPointer3+0
    sta $2006

    tya
    eor TmpX
    lsr a
    and #$01
    sta TmpX

    lda PPUBuffer, x
    bne :+
    lda TmpX
    ora #PlayfieldA
    sta $2007
    jmp :++

:
    and #$0F
    sta TmpY
    lda TmpX
    eor #$01
    asl a
    asl a
    asl a
    asl a
    ora TmpY
    sta $2007
:
    inx

    cpx BufferIdx
    beq @bufferDone

    cpx #(10*3)
    beq @bufferDone
    jmp @loop

@bufferDone:
    lda #0
    sta $2005
    sta $2005

    lda #%1000_0000
    sta $2000

    lda #$FF
    ldx #0
:
    sta PPUBuffer, x
    inx
    sta PPUBuffer, x
    inx
    sta PPUBuffer, x
    inx
    cpx #(10*3)
    bne :-

    sta BufferIdx

    pla
    tay
    pla
    tax
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

    jsr DrawScreen

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

    ;jsr ResetSpritePointers
    jsr ResetPlayfield
    jsr RedrawPlayfield

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

    lda #$FF
    sta BufferIdx

    lda #0
    sta $2005
    sta $2005

    lda #%1000_0000
    sta $2000

    lda #%0001_1110
    sta $2001

    lda #1
    sta IsPaused

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

:   jsr ClearSprites

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

    lda #0
    sta IsPaused

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
    ora #$30
    sta CountSprites+(4*i)+1
    .endrepeat

    ; Find next direction
    jsr ReadControllers

    lda #BUTTON_START ; start
    jsr ButtonPressed
    beq :+

    lda #1
    sta IsPaused
    jsr WaitForNMI
    jmp FramePaused
:
    lda #BUTTON_UP ; up
    jsr ButtonPressed
    beq :+
    lda Direction
    cmp #Dir::Down
    beq :+
    lda #Dir::Up
    sta NextDirection
:
    lda #BUTTON_RIGHT ; right
    jsr ButtonPressed
    beq :+
    lda Direction
    cmp #Dir::Left
    beq :+
    lda #Dir::Right
    sta NextDirection
:
    lda #BUTTON_DOWN ; down
    jsr ButtonPressed
    beq :+
    lda Direction
    cmp #Dir::Up
    beq :+
    lda #Dir::Down
    sta NextDirection
:
    lda #BUTTON_LEFT ; left
    jsr ButtonPressed
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
    ldx SnekTailY
    lda PlayfieldMemLo, x
    sta AddressPointer+0
    lda PlayfieldMemHi, x
    sta AddressPointer+1

    ldy SnekTailX
    lda (AddressPointer), y
    and #$0F
    sta PrevTail

    ; clear out old tail
    lda #0
    sta (AddressPointer), y

    ; buffer the write
    ldy BufferIdx
    bpl :+
    iny
:

    lda SnekTailX
    sta PPUBuffer, y
    iny

    lda SnekTailY
    sta PPUBuffer, y
    iny

    lda #0
    sta PPUBuffer, y
    iny
    sty BufferIdx

    ; figure out new X/Y
    lda PrevTail
    and #$03
    ;sta PrevTail
    bne :+
    ; Up
    dec SnekTailY
    jmp @tailMathDone

:   cmp #1
    bne :+
    ; right
    inc SnekTailX
    jmp @tailMathDone

:   cmp #2
    bne :+
    ; down
    inc SnekTailY
    jmp @tailMathDone

:   ; left
    dec SnekTailX

@tailMathDone:
    ; write the new tail in the correct direction
    ldx SnekTailY
    lda PlayfieldMemLo, x
    sta AddressPointer+0
    lda PlayfieldMemHi, x
    sta AddressPointer+1

    ldy SnekTailX
    lda (AddressPointer), y
    and #$0F
    sta NextTail
    cmp #SNEK::Vertical
    beq :+
    cmp #SNEK::Horizontal
    bne @tailCorners

:
    lda PrevTail
    ora #$10
    sta (AddressPointer), y
    pha

    ldy BufferIdx
    lda SnekTailX
    sta PPUBuffer, y
    iny

    lda SnekTailY
    sta PPUBuffer, y
    iny

    pla
    sta PPUBuffer, y
    iny
    sty BufferIdx

    jmp @tailDone

@tailCorners:
    pha
    lda PrevTail
    and #$03
    sta PrevTail
    pla

    ;
    ; tail tile in A, AND'd with $0F
    cmp #SNEK::Corner_TR    ; Corner_TR
    bne :++
    lda PrevTail
    cmp #Dir::Right         ; Right
    bne :+
    ; right -> down
    ldx #SNEK::Butt_Down    ; Butt_Down
    jmp @tailWrite

    ; up -> left
:   ldx #SNEK::Butt_Left    ; Butt_Left
    jmp @tailWrite

:   cmp #SNEK::Corner_TL    ; Corner_TL
    bne :++
    lda PrevTail
    cmp #Dir::Left          ; Left
    bne :+
    ; left -> down
    ldx #SNEK::Butt_Down    ; Butt_Down
    jmp @tailWrite

    ; up -> right
:   ldx #SNEK::Butt_Right   ; Butt_Right
    jmp @tailWrite

:   cmp #SNEK::Corner_BR    ; Corner_BR
    bne :++
    lda PrevTail
    cmp #Dir::Right         ; Right
    bne :+
    ; right -> up
    ldx #SNEK::Butt_Up      ; Butt_Up
    jmp @tailWrite

:   ; down -> left
    ldx #SNEK::Butt_Left    ; Butt_Left
    jmp @tailWrite

:   ;bottom left corner
    lda PrevTail
    cmp #Dir::Left          ; Left
    bne :+
    ; left -> up
    ldx #SNEK::Butt_Up      ; Butt_Up
    jmp @tailWrite

:   ; down -> right
    ldx #SNEK::Butt_Right   ; Butt_Right

@tailWrite:
    txa
    pha
    ldx SnekTailY
    lda PlayfieldMemLo, x
    sta AddressPointer+0
    lda PlayfieldMemHi, x
    sta AddressPointer+1

    ldy SnekTailX
    pla
    pha
    ora #$10
    sta (AddressPointer), y

    ldy BufferIdx
    lda SnekTailX
    sta PPUBuffer, y
    iny

    lda SnekTailY
    sta PPUBuffer, y
    iny

    pla
    sta PPUBuffer, y
    iny
    sty BufferIdx

@tailDone:

; Move Head
    ldx SnekHeadY
    lda PlayfieldMemLo, x
    sta AddressPointer+0
    lda PlayfieldMemHi, x
    sta AddressPointer+1

    ldy SnekHeadX
    lda (AddressPointer), y
    and #$0F
    sta PrevHead

    lda SnekHeadX
    sta PrevX
    lda SnekHeadY
    sta PrevY

    lda Direction
    and #$03
    cmp #Dir::Up
    bne :+
    lda #SNEK::Head_Up
    sta NextHead
    dec SnekHeadY
    jmp @dirDone

:   cmp #Dir::Right
    bne :+
    lda #SNEK::Head_Right
    sta NextHead
    inc SnekHeadX
    jmp @dirDone

:   cmp #Dir::Down
    bne :+
    lda #SNEK::Head_Down
    sta NextHead
    inc SnekHeadY
    jmp @dirDone

:   lda #SNEK::Head_Left
    sta NextHead
    dec SnekHeadX

@dirDone:
    ldx SnekHeadY
    lda PlayfieldMemLo, x
    sta AddressPointer+0
    lda PlayfieldMemHi, x
    sta AddressPointer+1

    ; Look at next tile, make sure it isn't a
    ; snek or food
    ldy SnekHeadX
    lda (AddressPointer), y
    bpl :+
    ; found an item
    inc Elongate
    jmp @update

:
    beq @update
    jmp Collide

@update:
    ; no collide, write tiles
    ; New head
    lda NextHead
    pha
    ora #$10
    sta (AddressPointer), y

    ldy BufferIdx
    bpl :+
    iny
:
    lda SnekHeadX
    sta PPUBuffer, y
    iny

    lda SnekHeadY
    sta PPUBuffer, y
    iny

    pla
    sta PPUBuffer, y
    iny
    sty BufferIdx

    ; remove old head
    lda PrevHead
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
    pha

    ldx PrevY
    lda PlayfieldMemLo, x
    sta AddressPointer+0
    lda PlayfieldMemHi, x
    sta AddressPointer+1

    pla
    pha
    ldy PrevX
    ora #$10
    sta (AddressPointer), y

    jmp @headDone

@straight:
    ; need the A/B color for the background
    and #1
    ora #SNEK::Vertical
    pha

    ldx PrevY
    lda PlayfieldMemLo, x
    sta AddressPointer+0
    lda PlayfieldMemHi, x
    sta AddressPointer+1

    pla
    pha
    ldy PrevX
    ora #$10
    sta (AddressPointer), y

@headDone:
    ldy BufferIdx

    lda PrevX
    sta PPUBuffer, y
    iny

    lda PrevY
    sta PPUBuffer, y
    iny

    pla
    ora #$10
    sta PPUBuffer, y
    iny
    sty BufferIdx

    lda SnekHeadY
    bmi :+
    cmp #Playfield_Height
    beq :+
    bcc :++
:   jmp Collide
:

    lda SnekHeadX
    bmi :+
    cmp #Playfield_Width
    beq :+
    bcc :++
:   jmp Collide
:

    lda Elongate
    beq :+
    jsr NextSprite
:
    jsr WaitForNMI
    jmp Frame

FramePaused:
    jsr ReadControllers

    lda #BUTTON_START
    jsr ButtonPressed
    beq :+

    lda #0
    sta IsPaused
    jsr WaitForNMI
    jmp Frame
:

    jsr WaitForNMI
    jmp FramePaused

Collide:
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

DrawScreen:
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

ResetPlayfield:
    lda #0
    ldx #0
    ldy #140
@clearLoop:
    sta Playfield, x
    sta Playfield+140, x
    sta Playfield+280, x
    sta Playfield+420, x
    inx
    dey
    bne @clearLoop

    lda #SNEK::Butt_Right
    sta Playfield+(1*28)+1
    lda #SNEK::Horizontal
    sta Playfield+(1*28)+2
    lda #SNEK::Head_Right
    sta Playfield+(1*28)+3
    ;               Y    X

    lda #1
    sta SnekHeadY
    sta SnekTailX
    sta SnekTailY

    lda #3
    sta SnekHeadX

    jsr ResetSpritePointers
    jsr NextSprite

    rts

RedrawPlayfield:

    bit $2002

    lda #0
    sta TmpY
@rowLoop:
    lda #0
    sta TmpX
    ldx TmpY

    lda PlayfieldMemLo, x
    sta AddressPointer+0

    lda PlayfieldMemHi, x
    sta AddressPointer+1

    ldy #0

@colLoop:
    lda (AddressPointer), y
    beq @nextCol
    bmi @nextCol
    sta TmpA

    clc
    lda PlayfieldAddrLo, x
    adc TmpX
    sta AddressPointer2+0
    lda PlayfieldAddrHi, x
    adc #0
    sta AddressPointer2+1

    lda TmpX
    lsr a
    and #$01
    sta TmpZ

    lda TmpY
    lsr a
    and #$01
    eor TmpZ
    beq @dark
    lda #$00
    jmp :+
@dark:
    lda #$10
:
    ora TmpA
    sta TmpZ

    lda AddressPointer2+1
    sta $2006
    lda AddressPointer2+0
    sta $2006
    lda TmpZ
    sta $2007

@nextCol:
    inc AddressPointer+0
    bne :+
    inc AddressPointer+1
:
    inc TmpX
    lda TmpX
    cmp #28
    bne @colLoop

    ; next row
    inc TmpY
    lda TmpY
    cmp #20
    bne @rowLoop
    rts

    clc
    ldy SnekHeadY
    lda PlayfieldMemLo, y
    adc SnekHeadX
    sta AddressPointer+0

    lda PlayfieldMemHi, y
    adc #0
    sta AddressPointer+1

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
    ;jmp BinToDec
    rts

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
    beq :+  ; make sure we have an empty tile
    jsr IncSpritePointers
    jmp @next
:

    lda #SpriteId
    sta FoodSprite+1
    lda #2
    sta FoodSprite+2

    lda #$80
    sta (AddressPointer), y
    lda (SpriteCoordPointer), y
    sta FoodSprite+0
    iny
    lda (SpriteCoordPointer), y
    sta FoodSprite+3

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
    ;jmp BinToDec
    rts

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

;ClearLogo:
ClearSprites:
    lda #$FF
    ldx #0
:
    sta LogoSprites+0, x
    inx
    sta LogoSprites+0, x
    inx
    sta LogoSprites+0, x
    inx
    sta LogoSprites+0, x
    inx

    cpx #(OtherSpriteCount*4)-4
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
    sta FoodSprite+0
    rts

GamePalette_BG:
    .byte $19, $21, $11, $01
    .byte $19, $09, $17, $27
    .byte $19, $19, $0C, $1C
    .byte $19, $0F, $0F, $31

GamePalette_SP:
    .byte $19, $20, $17, $27
    .byte $19, $20, $10, $35
    .byte $19, $20, $10, $35
    .byte $19, $15, $25, $35
    ;.byte $19, $15, $25, $35

PausedPalette_BG:
    .byte $10, $00, $00, $2D
    .byte $10, $00, $0F, $10
    ;.byte $00, $06, $16, $26
    .byte $10, $0F, $0F, $0F
    .byte $10, $0F, $0F, $0F

PausedPalette_SP:
    .byte $10, $10, $17, $27
    .byte $10, $17, $10, $0F
    .byte $10, $00, $0F, $0F
    .byte $10, $0F, $0F, $0F

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

PlayfieldMemHi:
    .repeat 20, i
        .byte .hibyte($0400+(i*28))
    .endrepeat

PlayfieldMemLo:
    .repeat 20, i
        .byte .lobyte($0400+(i*28))
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
    .include "rand.inc"
    .word 0

SpriteCoords:
    .include "coords.inc"
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
