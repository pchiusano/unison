#!/bin/sh
# Builds and runs the LLVM spike with the project's GHC. Run from the repo root or from here.
set -e
cd "$(dirname "$0")"
LLVM_CONFIG=${LLVM_CONFIG:-/opt/homebrew/opt/llvm/bin/llvm-config}
INC=$($LLVM_CONFIG --includedir)
LIB=$($LLVM_CONFIG --libdir)
mkdir -p build
stack ghc -- -O2 -threaded -outputdir build -o build/llvm-spike \
  Main.hs shim.c \
  -optc-I"$INC" -L"$LIB" $($LLVM_CONFIG --libs --link-shared) \
  -optl-Wl,-rpath,"$LIB"
./build/llvm-spike
