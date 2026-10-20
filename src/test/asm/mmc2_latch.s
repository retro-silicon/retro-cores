; MMC2/MMC4 latch exercise for the RTL cosim (no verdict of its own): background rows of tiles
; FD and FE in both pattern tables, sprites using them too, bank registers rewritten mid-frame,
; and 2007 reads of 0FD8/0FE8/1FD8/1FE8 outside rendering. After 120 frames it reports "done"
; through blargg's 6000 protocol, so the cosim's verdict is whether the RTL matched the model.
; Build: scripts/make-nes-mapper-tests.sh
.segment "HEADER"
  .byte "NES", $1A, 2, 4, ((MAPPER & $0F) << 4) | 1, MAPPER & $F0, 0,0,0,0,0,0,0,0
.segment "ZEROPAGE"
frame: .res 1

.segment "CODE"
reset:
  sei
  ldx #$FF
  txs
  lda #0
  sta $2000
  sta $2001
: bit $2002
  bpl :-
: bit $2002
  bpl :-
  ; 6000 status: running
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
  ; banks: FD/FE low 0/1, FD/FE high 2/3, vertical mirroring
  lda #0
  sta $B000
  lda #1
  sta $C000
  lda #2
  sta $D000
  lda #3
  sta $E000
  lda #0
  sta $F000
  ; nametable 2000: rows alternate FD FE 00 01 ...
  lda #$20
  sta $2006
  lda #0
  sta $2006
  ldy #0
  ldx #4
fill:
  tya
  and #3
  clc
  adc #$FC
  sta $2007
  iny
  bne fill
  dex
  bne fill
  ; palette
  lda #$3F
  sta $2006
  lda #0
  sta $2006
  ldx #0
: txa
  sta $2007
  inx
  cpx #32
  bne :-
  ; sprites: 64 sprites of tiles FD/FE spread over the screen
  ldx #0
: txa
  asl
  asl
  sta $0200,x     ; y
  txa
  and #1
  ora #$FD
  sta $0201,x     ; tile
  lda #0
  sta $0202,x
  txa
  asl
  asl
  asl
  sta $0203,x     ; x
  inx
  inx
  inx
  inx
  bne :-
  lda #2
  sta $4014
  lda #0
  sta $2005
  sta $2005
  lda #%10001000  ; NMI, sprites from 1000
  sta $2000
  lda #%00011110
  sta $2001
  lda #0
  sta frame
loop:
  ; rewrite a bank register mid-frame every so often
  lda frame
  and #7
  sta $B000
  lda frame
  lsr
  and #7
  sta $E000
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
: lda #0
  sta $2001
  ; 2007 reads at the latch addresses, outside rendering
  lda frame
  and #3
  tax
  lda hi,x
  sta $2006
  lda lo,x
  sta $2006
  lda $2007
  lda $2007
  lda #2
  sta $4014
  lda #0
  sta $2006
  sta $2006
  sta $2005
  sta $2005
  lda frame
  and #$10
  ora #%10001000
  sta $2000
  lda #%00011110
  sta $2001
  pla
  tax
  pla
irq:
  rti
done: .byte "done", 0
hi: .byte $0F, $0F, $1F, $1F
lo: .byte $D8, $E8, $D8, $E8

.segment "VECTORS"
  .addr nmi, reset, irq
