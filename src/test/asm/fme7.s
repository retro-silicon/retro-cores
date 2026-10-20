; Sunsoft FME-7 exercise for the RTL cosim (no verdict of its own): the eight 1 KB CHR banks
; and the mirroring rewritten every frame while rendering, the three PRG banks and 6000-7FFF
; as ROM, disabled RAM (open bus) and RAM, and the IRQ counter firing many times a frame, at any instruction,
; acknowledged and reloaded by its handler. After 120 frames it reports "done" through
; blargg's 6000 protocol. Build: scripts/make-nes-mapper-tests.sh
.segment "HEADER"
  .byte "NES", $1A, 2, 4, $50, $40, 0,0,0,0,0,0,0,0

.segment "BANK0"
  .byte $B0, "bank 0"
.segment "BANK1"
  .byte $B1, "bank 1"
.segment "BANK2"
  .byte $B2, "bank 2"

.segment "ZEROPAGE"
frame: .res 1
irqs: .res 1
reads: .res 8

.segment "CODE"
.macro fme7 cmd, value
  lda #cmd
  sta $8000
  lda value
  sta $A000
.endmacro

reset:
  sei
  ldx #$FF
  txs
  lda #$40
  sta $4017         ; no APU frame IRQ
  lda #0
  sta $2000
  sta $2001
  sta frame
  sta irqs
: bit $2002
  bpl :-
: bit $2002
  bpl :-
  fme7 8, #$C0      ; RAM at 6000
  lda #$80
  sta $6000
  lda #$DE
  sta $6001
  lda #$B0
  sta $6002
  lda #$61
  sta $6003
  lda #0
  sta $6004
  ldx #0
: txa
  pha
  stx $8000
  asl
  asl
  asl
  sta $A000         ; CHR bank i: 8 i
  pla
  tax
  inx
  cpx #8
  bne :-
  fme7 $C, #0
  ; nametables: a ramp of tiles
  lda #$20
  sta $2006
  lda #0
  sta $2006
  ldx #8
  ldy #0
: sty $2007
  iny
  bne :-
  dex
  bne :-
  lda #$3F
  sta $2006
  lda #0
  sta $2006
  ldx #0
: stx $2007
  inx
  cpx #32
  bne :-
  ; the IRQ every 3000 cycles
  fme7 $E, #<3000
  fme7 $F, #>3000
  fme7 $D, #$81
  lda #0
  sta $2005
  sta $2005
  lda #$80
  sta $2000
  lda #$1E
  sta $2001
  cli
loop:
  sei
  ldx #0
  ; PRG banks at 8000, A000, C000
: txa
  pha
  and #3
  cmp #3
  bne :+
  lda #0
: tay
  lda #9
  sta $8000
  sty $A000
  lda #$A
  sta $8000
  sty $A000
  lda #$B
  sta $8000
  sty $A000
  lda $8000
  eor $A001
  eor $C002
  sta reads
  pla
  tax
  inx
  cpx #6
  bne :--
  ; 6000 as ROM bank 1, as disabled RAM, then as RAM again
  fme7 8, #1
  lda $6000
  sta reads+1
  fme7 8, #$40
  lda $6005
  sta reads+2
  fme7 8, #$C0
  lda $6001
  sta reads+3
  cli
  ; most of the time IRQs can come, at any instruction
: lda reads,x
  inx
  bne :-
  jmp loop

nmi:
  pha
  txa
  pha
  inc frame
  lda frame
  cmp #120
  bne :++
  ldx #0
: lda done,x
  sta $6004,x
  inx
  cpx #5
  bne :-
  lda #0
  sta $6000
: ; CHR bank 0 and the mirroring follow the frame
  fme7 0, frame
  lda frame
  lsr
  lsr
  lsr
  and #3
  tax
  lda #$C
  sta $8000
  stx $A000
  pla
  tax
  pla
  rti

irq:
  pha
  inc irqs
  lda #$E
  sta $8000
  lda irqs
  ora #$40
  sta $A000
  lda #$F
  sta $8000
  lda #0
  sta $A000
  lda #$D
  sta $8000
  lda #$81
  sta $A000
  ; CHR bank 4 follows the IRQs, mid-frame
  lda #4
  sta $8000
  lda irqs
  sta $A000
  pla
  rti

done: .byte "done", 0

.segment "VECTORS"
  .addr nmi, reset, irq
