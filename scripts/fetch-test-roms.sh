#!/bin/sh
# Downloads the freely distributable Game Boy test ROMs into ./roms (gitignored):
# Blargg's CPU tests (gbdev.gg8.se mirror of the originals) and the Mooneye Test Suite (MIT).
# `sbt "testOnly retro.gb.GameBoyRomSim"` runs them; without ./roms that suite is skipped.
set -eu
cd "$(dirname "$0")/.."
mkdir -p roms
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
for t in cpu_instrs instr_timing mem_timing mem_timing-2 halt_bug; do
  curl -fsSL -o "$tmp/$t.zip" "https://gbdev.gg8.se/files/roms/blargg-gb-tests/$t.zip"
  mkdir -p "roms/blargg/$t" && unzip -qo "$tmp/$t.zip" -d "roms/blargg/$t"
done
mts=mts-20240926-1737-443f6e1
curl -fsSL -o "$tmp/mts.zip" "https://gekkio.fi/files/mooneye-test-suite/$mts/$mts.zip"
unzip -qo "$tmp/mts.zip" -d "$tmp" && rm -rf roms/mooneye && mv "$tmp/$mts" roms/mooneye
find roms -name '*.gb' | wc -l
