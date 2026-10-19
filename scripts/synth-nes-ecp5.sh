#!/bin/sh
# Synthesizes, places and routes the NES (NROM cartridge, no trace taps) for an ECP5-85F with
# the open toolchain, and prints its size and maximum clock. Uses yosys and nextpnr-ecp5, or
# their YoWASP builds (pip install yowasp-yosys yowasp-nextpnr-ecp5).
set -eu
cd "$(dirname "$0")/.."
sbt "runMain retro.nes.NesVerilog"
cd gen
yosys=$(command -v yosys || command -v yowasp-yosys)
nextpnr=$(command -v nextpnr-ecp5 || command -v yowasp-nextpnr-ecp5)
"$yosys" -q -l yosys.log -p "read_verilog Nes.v; synth_ecp5 -top Nes -json nes.json"
"$nextpnr" --85k --package CABGA381 --json nes.json --freq 10 --log pnr.log -q
grep -E "TRELLIS_(COMB|FF|RAMW)|MULT18X18D|DP16KD" pnr.log | tail -5
grep "Max frequency" pnr.log | tail -1
