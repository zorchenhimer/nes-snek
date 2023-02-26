
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

.segment "OAM"
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

    lda #$00
    sta $2003
    lda #$02
    sta $4014

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

    ldy #0
@loop:
    lda (AddressPointer), y
    cmp #$FF
    beq @done
    sta $2007
    iny
    bne @loop
    inc AddressPointer+1
    jmp @loop
@done:

    lda #.hibyte(PlayfieldAttr)
    sta AddressPointer+1
    lda #.lobyte(PlayfieldAttr)
    sta AddressPointer+0

    lda #$23
    sta $2006
    lda #$C0
    sta $2006

    ldy #0
@loopAttr:
    lda (AddressPointer), y
    sta $2007
    iny
    cpy #64
    bne @loopAttr
    rts

PlayfieldData:
    .include "playfield.i"

PlayfieldAttr:
    .repeat 64
        .byte $00
    .endrepeat
