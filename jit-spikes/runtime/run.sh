#!/bin/sh
# Builds and runs the runtime spike with the project's GHC, against the real unison-runtime
# package. Needs the project to be built first (`stack build`). Run from anywhere.
#
#   run.sh          all tests, normal runtime
#   run.sh debug    all tests, debug runtime with heap sanity checks and a tiny nursery
#   run.sh nomark   the list test with array marking left out, which should fail
set -e
cd "$(dirname "$0")"
MODE=${1:-normal}
mkdir -p build
WAY=""
RTS="-T -N2"
if [ "$MODE" = "debug" ] || [ "$MODE" = "nomark" ]; then
  WAY="-debug"
  RTS="-T -N2 -DS -A64k"
fi
stack ghc --package unison-runtime --package unison-parser-typechecker --package unison-core1 --package primitive -- \
  -O1 -threaded -rtsopts $WAY -outputdir build/$MODE -o build/spike-$MODE Main.hs rt.c
ARGS=""
[ "$MODE" = "nomark" ] && ARGS="nomark"
./build/spike-$MODE $ARGS +RTS $RTS -RTS
