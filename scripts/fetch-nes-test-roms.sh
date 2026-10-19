#!/bin/sh
# Downloads freely distributed NES test ROMs into ./roms (gitignored):
# - roms/nes: nestest by Kevin Horton, with the Nintendulator log the RTL is compared against;
# - roms/nes-test-roms: blargg's CPU and PPU tests from the christopherpow/nes-test-roms collection.
# `sbt "testOnly retro.r2a03.R2A03NestestSim"` and `sbt "Test/runMain retro.nes.NesRefRom"` use them.
set -eu
cd "$(dirname "$0")/.."
mkdir -p roms/nes
for f in nestest.nes nestest.log nestest.txt; do
  curl -sSfL -o "roms/nes/$f" "https://www.qmtpro.com/~nes/misc/$f"
done
rm -rf roms/nes-test-roms roms/.nes-test-roms
git clone -q --depth 1 https://github.com/christopherpow/nes-test-roms.git roms/.nes-test-roms
mkdir -p roms/nes-test-roms
for d in ppu_vbl_nmi vbl_nmi_timing sprite_hit_tests_2005.10.05 sprite_overflow_tests oam_read oam_stress \
         ppu_open_bus ppu_read_buffer blargg_ppu_tests_2005.09.15b cpu_interrupts_v2 instr_test-v5 instr_timing \
         instr_misc cpu_timing_test6 branch_timing_tests cpu_dummy_reads sprdma_and_dmc_dma; do
  cp -r "roms/.nes-test-roms/$d" roms/nes-test-roms/
done
rm -rf roms/.nes-test-roms
find roms/nes-test-roms -name '*.nes' | wc -l
