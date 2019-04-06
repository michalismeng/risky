# risky

A 32-bit risc-v processor implementation in [clash](https://clash-lang.org/).

## Features

Encoding/Decoding of instructions of the RV32I specification.

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
stack exec -- clash src/System.hs -isrc --verilog -odir obj -hidir obj
```

### Test Suite
```
stack exec -- runhaskell test/Spec.hs
```