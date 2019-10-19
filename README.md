# risky

A 32-bit risc-v processor implementation in [clash](https://clash-lang.org/).

## Features

Implementation of RV32I specification

5-Stage pipeline with forwarding

Not implemented yet
- fence / fence.I
- ecall
- ebreak
- csr*

Jumps (jal, jalr, branches) 
- stall on decode to avoid fetching bad instruction
- stall and resolve on execute

## How to install

Clone the project and then execute:
```
stack build
```

This will install the clash environment and all the required haskell packages.

## How to run

### REPL
```
stack exec -- clashi src/System.hs -isrc -odir obj -hidir obj
```

### Generate verilog
```
stack exec -- clash src/System.hs -i{src,test} --verilog -odir obj -hidir obj
```

### Generate waveforms (gtkwave)
```
stack exec -- clash src/System.hs -i{src,test} --verilog -odir obj -hidir obj
iverilog verilog/cpu.v verilog/System/*.v -o verilog/cpu
vvp verilog/cpu
gtkwave verilog/dump.vcd
```

### Test Suite
```
stack exec -- runhaskell test/Spec.hs
```