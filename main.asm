
.include "nes2header.inc"
nes2mapper 0
nes2prg 1 * 8 * 1024
nes2chr 1 * 8 * 1024
nes2mirror 'V'
nes2tv 'N'
nes2end

.feature leading_dot_in_identifiers
.feature underline_in_numbers
.feature addrsize

.segment "ZEROPAGE"
.segment "OAM"
.segment "BSS"

.segment "VECTORS"

.segment "CHR0"
.segment "CHR1"

.segment "PAGE0"

