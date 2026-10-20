#!/bin/sh
# Downloads the freely distributable Game Boy test ROMs into ./roms (gitignored):
# Blargg's CPU tests (gbdev.gg8.se mirror of the originals), the Mooneye Test Suite (MIT) and
# dmg-acid2 (MIT) with its reference image; and from c-sp/game-boy-test-roms (MIT) the DMG-relevant
# suites with their reference screenshots: Mealybug Tearoom Tests, AGE, Bully, Scribbltests and
# Strikethrough (MIT), Gambatte (GPLv2), GBMicrotest (MIT), wilbertpol's Mooneye fork (GPLv3),
# little-things-gb (zlib), SameSuite (X11), rtc3test (public domain), MBC3 Tester and TurtleTests
# (no license stated).
# `sbt "testOnly retro.gb.GameBoyRomSim"` runs them; without ./roms that suite is skipped.
set -eu
cd "$(dirname "$0")/.."
mkdir -p roms
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
for t in cpu_instrs instr_timing mem_timing mem_timing-2 halt_bug oam_bug; do
  curl -fsSL -o "$tmp/$t.zip" "https://gbdev.gg8.se/files/roms/blargg-gb-tests/$t.zip"
  mkdir -p "roms/blargg/$t" && unzip -qo "$tmp/$t.zip" -d "roms/blargg/$t"
done
mts=mts-20240926-1737-443f6e1
curl -fsSL -o "$tmp/mts.zip" "https://gekkio.fi/files/mooneye-test-suite/$mts/$mts.zip"
unzip -qo "$tmp/mts.zip" -d "$tmp" && rm -rf roms/mooneye && mv "$tmp/$mts" roms/mooneye
mkdir -p roms/dmg-acid2
curl -fsSL -o roms/dmg-acid2/dmg-acid2.gb https://github.com/mattcurrie/dmg-acid2/releases/download/v1.0/dmg-acid2.gb
curl -fsSL -o roms/dmg-acid2/reference-dmg.png https://raw.githubusercontent.com/mattcurrie/dmg-acid2/master/img/reference-dmg.png
v=7.0
curl -fsSL -o "$tmp/c-sp.zip" "https://github.com/c-sp/game-boy-test-roms/releases/download/v$v/game-boy-test-roms-v$v.zip"
unzip -qo "$tmp/c-sp.zip" -d "$tmp/c-sp" && rm -rf roms/c-sp && mkdir -p roms/c-sp
for s in mealybug-tearoom-tests age-test-roms bully gambatte gbmicrotest little-things-gb mbc3-tester \
    blargg mooneye-test-suite-wilbertpol rtc3test same-suite scribbltests strikethrough turtle-tests; do
  mv "$tmp/c-sp/$s" roms/c-sp/
done
find roms -name '*.gb' | wc -l
