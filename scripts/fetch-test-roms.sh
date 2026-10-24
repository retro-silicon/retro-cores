#!/bin/sh
# Downloads the freely distributable Game Boy test ROMs into ./roms/c-sp (gitignored): from
# c-sp/game-boy-test-roms (MIT) the DMG-relevant suites with their reference screenshots: Blargg's
# tests, the Mooneye Test Suite (MIT), dmg-acid2, Mealybug Tearoom Tests, AGE, Bully, Scribbltests
# and Strikethrough (MIT), Gambatte (GPLv2), GBMicrotest (MIT), wilbertpol's Mooneye fork (GPLv3),
# little-things-gb (zlib), SameSuite (X11), rtc3test (public domain), MBC3 Tester and TurtleTests
# (no license stated).
# `sbt "testOnly retro.gb.GbSuitesSim"` runs them; without ./roms that suite is skipped.
set -eu
cd "$(dirname "$0")/.."
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
v=7.0
curl -fsSL -o "$tmp/c-sp.zip" "https://github.com/c-sp/game-boy-test-roms/releases/download/v$v/game-boy-test-roms-v$v.zip"
unzip -qo "$tmp/c-sp.zip" -d "$tmp/c-sp" && rm -rf roms/c-sp && mkdir -p roms/c-sp
for s in blargg mooneye-test-suite dmg-acid2 mealybug-tearoom-tests age-test-roms bully gambatte gbmicrotest \
    little-things-gb mbc3-tester mooneye-test-suite-wilbertpol rtc3test same-suite scribbltests strikethrough turtle-tests; do
  mv "$tmp/c-sp/$s" roms/c-sp/
done
find roms/c-sp -name '*.gb' | wc -l
