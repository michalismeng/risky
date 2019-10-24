#!/usr/bin/python3

# Script that assembles a risc-v assembly file and generates the instruction and data streams in Haskell code.
# The streams are saved in the 'Program.hs' file as BitVector 32 vectors and are used by the top-level 'System.hs' file 
# to initialize the instruction and data memories.

import sys
import os
import tempfile
import binascii
import array

def changeOrder (s):
    return s[6:8] + s[4:6] + s[2:4] + s[0:2]


def haskellPrelude (sourceFile):
    return "-- Auto-generated file. [Source = %s].\n-- Do not alter.\n\n{-# LANGUAGE NoImplicitPrelude, DataKinds, BinaryLiterals #-}\nmodule Configuration.Generated.Program where\n\nimport Clash.Prelude" % sourceFile

def toHaskell (s):
    return "  (%s :: BitVector 32) :>" % s

flatten = lambda l: [item for sublist in l for item in sublist]

def formatSectionAsHaskell (section, name, maxLength = -1):
    lines = section.splitlines()                            # split lines
    lines = filter (lambda s: '0x' in s, lines)
    lines = map(lambda line: line.split(' '), lines)
    lines = map(lambda line: list(filter(lambda s: s != "" and '.' not in s, line)), lines)
    lines = list(lines)
    lines = map(lambda line: line[1:], lines)
    sectionData = flatten (lines)

    if maxLength != -1:
        sectionData = list(sectionData)[0:maxLength]

    sectionData = map (lambda s: '0x' + changeOrder(s), sectionData)
    sectionData = map (lambda s: toHaskell(s), sectionData)

    return "%s = \n%s\n  Nil" % (name, '\n'.join(sectionData))

if len(sys.argv) <= 1:
    print ("Please provide the name of the file to assemble!")
    exit (1)

in_file = sys.argv[1]
temp = tempfile.NamedTemporaryFile()
temp2 = tempfile.NamedTemporaryFile()

os.system('riscv32-unknown-linux-gnu-as -o %s %s' % (temp.name, in_file))
os.system('riscv32-unknown-linux-gnu-ld -Ttext 0 -Tdata 1000 -o %s %s' % (temp2.name, temp.name))
textSection = os.popen('riscv32-unknown-linux-gnu-readelf -x .text %s' % temp2.name).read()
dataSection = os.popen('riscv32-unknown-linux-gnu-readelf -x .data %s' % temp2.name).read()

textSectionHaskell = formatSectionAsHaskell(textSection, "instructionStream", 32)
dataSectionHaskell = formatSectionAsHaskell(dataSection, "dataStream", 64)

haskellProgram = "%s\n\n%s\n\n%s" % (haskellPrelude (in_file), textSectionHaskell, dataSectionHaskell)

with open("Generated/Program.hs", "w") as f:
    f.write(haskellProgram)

temp.close()
temp2.close()