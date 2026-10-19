#!/bin/sh
# Downloads the nes6502 set of SingleStepTests/65x02 (MIT): 10,000 single-instruction tests per
# 2A03 opcode, with the bus access of every cycle, into ./roms/sst-nes6502 (gitignored, about 1.1 GB).
# `sbt "testOnly retro.r2a03.R2A03SingleStepSim"` runs them; without them that suite is skipped.
set -eu
cd "$(dirname "$0")/.."
mkdir -p roms
rm -rf roms/sst-nes6502 roms/.sst-65x02
git clone -q --depth 1 --filter=blob:none --sparse https://github.com/SingleStepTests/65x02.git roms/.sst-65x02
git -C roms/.sst-65x02 sparse-checkout set --no-cone '/nes6502/v1/*'
mv roms/.sst-65x02/nes6502/v1 roms/sst-nes6502
rm -rf roms/.sst-65x02
ls roms/sst-nes6502 | wc -l
