#!/bin/sh
# Downloads freely distributed NES CPU test ROMs into ./roms/nes (gitignored):
# nestest by Kevin Horton, with the Nintendulator log the RTL is compared against.
# `sbt "testOnly retro.r2a03.R2A03NestestSim"` runs it; without them that test is skipped.
set -eu
cd "$(dirname "$0")/.."
mkdir -p roms/nes
for f in nestest.nes nestest.log nestest.txt; do
  curl -sSfL -o "roms/nes/$f" "https://www.qmtpro.com/~nes/misc/$f"
done
ls -l roms/nes
