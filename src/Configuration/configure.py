#!/usr/bin/python3

# Script that configures RAM and ROM sizes of the processor 

import sys

romSize = int (input ('Enter size of ROM in bytes (must be a multiple of 4): '))
ramSize = int (input ('Enter size of RAM in bytes (must be a multiple of 4): '))

haskellPrelude = "-- Auto-generated file.\n-- Do not alter.\n\n{-# LANGUAGE NoImplicitPrelude, DataKinds, BinaryLiterals #-}\nmodule Configuration.Generated.Materialize where\n\nimport Clash.Prelude"

def haskellMaterializeFunction (rom, ram):
    return "materialize iStream dStream = ((iStream ++ repeat 0) :: Vec (%d) (BitVector 32), (dStream ++ repeat 0) :: Vec (%d) (BitVector 32))" % (rom, ram)

haskellProgram = haskellPrelude + "\n\n" + haskellMaterializeFunction(romSize, ramSize)

with open ("Generated/Materialize.hs", "w") as f:
    f.write(haskellProgram)