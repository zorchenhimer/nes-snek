
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

TmpX: .res 1
TmpY: .res 1

.segment "OAM"
SpriteZero: .res 4
Sprites: .res (4 * 63)

.segment "BSS"

.segment "VECTORS0"
    .word NMI
    .word RESET
    .word IRQ

.segment "CHR0"
    .incbin "pattern-a.chr"

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

    lda #0
    sta $2005
    sta $2005

    lda #$88
    sta $2000

    lda #%0001_1110
    sta $2001

Frame:
    jsr WaitForNMI
    jmp Frame

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
@firstRow:
    lda (AddressPointer), y
    asl a
    asl a
    tax
    lda MetaTileIDs, x
    sta $2007
    lda MetaTileIDs+1, x
    sta $2007
    iny
    cpy #16
    bne @firstRow

    ldy #0
@secondRow:
    lda (AddressPointer), y
    asl a
    asl a
    tax
    lda MetaTileIDs+2, x
    sta $2007
    lda MetaTileIDs+3, x
    sta $2007
    iny
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
    tax
    lda MetaTileAttr, x
    sta TmpY
    iny

    lda (AddressPointer), y
    tax
    lda MetaTileAttr, x
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
    tax
    lda MetaTileAttr, x
    asl a
    asl a
    asl a
    asl a
    ora TmpY
    sta TmpY
    iny

    lda (AddressPointer), y
    tax
    lda MetaTileAttr, x
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

Palette_BG:
    .byte $01, $11, $21, $31
    .byte $01, $09, $0C, $1C
    .byte $01, $19, $0C, $1C
    .byte $01, $0F, $0F, $31

Palette_SP:
    .byte $01, $15, $25, $35
    .byte $01, $15, $25, $35
    .byte $01, $15, $25, $35
    .byte $01, $15, $25, $35

MetaTileIDs:
    .byte 0, 1, 0, 1
    .byte 0, 0, 7, 7
    .byte 2, 2, 0, 0
    .byte 3, 0, 3, 0
    .byte 4, 0, 0, 0
    .byte 0, 5, 0, 0
    .byte 0, 0, 0, 8
    .byte 0, 0, 9, 0
    .byte 0, 0, 0, 0

    .byte 0, 0, 0, 0
    .byte 6, 6, 6, 6

MetaTileAttr:
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0

    ; Tiled playfield
    .byte 1
    .byte 2

PlayfieldData:
    .include "playfield.i"
