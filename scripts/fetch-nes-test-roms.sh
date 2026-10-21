#!/bin/sh
# Downloads freely distributed NES test ROMs into ./roms (gitignored):
# - roms/nes: nestest by Kevin Horton, with the Nintendulator log the RTL is compared against;
# - roms/nes-test-roms: blargg's CPU, PPU, APU and MMC3 tests from the christopherpow/nes-test-roms collection.
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
         instr_misc cpu_timing_test6 branch_timing_tests cpu_dummy_reads sprdma_and_dmc_dma \
         apu_test blargg_apu_2005.07.30 apu_mixer apu_reset dmc_dma_during_read4 cpu_reset \
         mmc3_test mmc3_test_2 mmc3_irq_tests blargg_nes_cpu_test5 cpu_dummy_writes cpu_exec_space read_joy3 \
         pal_apu_tests nmi_sync; do
  cp -r "roms/.nes-test-roms/$d" roms/nes-test-roms/
done
# These wait for someone to press buttons.
rm roms/nes-test-roms/read_joy3/count_errors.nes roms/nes-test-roms/read_joy3/count_errors_fast.nes \
   roms/nes-test-roms/read_joy3/test_buttons.nes
rm -rf roms/.nes-test-roms
find roms/nes-test-roms -name '*.nes' | wc -l
