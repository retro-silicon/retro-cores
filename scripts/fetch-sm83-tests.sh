#!/bin/sh
# Downloads SingleStepTests/sm83 (MIT): 1000 single-instruction tests per SM83 opcode, with the
# bus state of every M-cycle, into ./roms/sst-sm83 (gitignored, about 190 MB).
# `sbt "testOnly retro.sm83.Sm83SingleStepSim"` runs them; without them that suite is skipped.
set -eu
cd "$(dirname "$0")/.."
mkdir -p roms
rm -rf roms/sst-sm83
git clone -q --depth 1 https://github.com/SingleStepTests/sm83.git roms/sst-sm83
ls roms/sst-sm83/v1 | wc -l
