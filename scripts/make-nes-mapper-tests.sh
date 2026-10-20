#!/bin/sh
# Builds the mapper exercise ROMs in src/test/asm (cc65's ca65 and ld65) into
# roms/nes-test-roms/retro-cores/: MMC2 and MMC4 latch ROMs and an FME-7 one, each 32 KB of
# PRG and 32 KB of CHR whose bytes depend on their bank and tile. They report "done" after 120
# frames, so their verdict is the RTL cosim's: whether the RTL matched the model.
set -eu
cd "$(dirname "$0")/.."
out=roms/nes-test-roms/retro-cores
mkdir -p "$out" target/asm
# Appends the CHR to a PRG image.
chr='
import sys
chr = bytes(((b * 16 + (t >> 4)) ^ (t << 1) ^ r) & 0xFF for b in range(8) for t in range(256) for r in range(16))
sys.stdout.buffer.write(open(sys.argv[1], "rb").read() + chr)'
for chip in 2:9 4:10; do
  mapper=${chip#*:}
  ca65 -D MAPPER=$mapper src/test/asm/mmc2_latch.s -o target/asm/mmc2_latch_$mapper.o
  ld65 -C src/test/asm/nes32k.cfg target/asm/mmc2_latch_$mapper.o -o target/asm/prg_$mapper.bin
  python3 -I -c "$chr" target/asm/prg_$mapper.bin > "$out/mmc${chip%:*}_latch.nes"
done
ca65 src/test/asm/fme7.s -o target/asm/fme7.o
ld65 -C src/test/asm/nes32k.cfg target/asm/fme7.o -o target/asm/prg_69.bin
python3 -I -c "$chr" target/asm/prg_69.bin > "$out/fme7.nes"
ls -l "$out"
