# Processor Simulator
Simulator of a pipelined, superscalar processor. 

# Install 

Clone the repo, and then run the following commands. The compiled output can be found in `dist/build/vm`.

```
> cd processor_sim
> cabal sandbox init
> cabal install
> cabal build
```

# Running
The processor runs binary files representing assembly instructions (see *Assembly Instructions*). To avoid having to write this manually, the C-- compiler is available [here](https://github.com/BakerSmithA/c--compiler) which outputs binary for the simulator. To run a compiled binary file, simply supply the filename as an argument to this simulator.

# Design
To run the processor, the following stages of the pipeline are run:

1. **Fetch**; *N* instructions (where *N* is the width of the pipeline) are fetched from the instruction cache.
2. **Decode**; instructions are decoded, and destination registers are renamed to remove false dependencies between instructions.
3. **Execute**; instructions are placed in reservation stations, and then run by their corresponding execution unit once all the instructions dependencies have been resolved.
4. **Commit**; instructions are placed in the reorder buffer. This allows instructions to be written back to memory or registers in the same order as they were fetched. This also allows speculatively executed instructions to be discarded.
5. **Writeback**; instructions are written back to memory or registers.

<img src="images/design.png" width="600">

# Assembly Instructions

The instruction set consists of 24 instructions. The notation used is:

| Symbol | Meaning |
|--------|---------|
| `r`    | Register at index `r` |
| `mem[x]` | Value of memory at index `x` |
| `#i` | Immediate with value `i` |
| `pc` | Program counter |
| `lr` | Link Register |

| Instruction Mnemonic | Action |
|-------------|--------|
| `LoadIdx r base #off` | `r <- mem[base + #off]` |
| `LoadBaseIdx r base off` | `r <- mem[base + off]` |
| `StoreIdx r base #off` | `mem[base + #off] <- r` |
| `StoreBaseIdx r base off` | `mem[base + off] <- r` |
||
| `MoveI r #i` | `r <- #i` |
| `Move r src` | `r <- src` |
| `Add r x y` | `r <- x + y` |
| `AddI r x #i` | `r <- x + #i` |
| `Sub r x y` | `r <- x - y` | 
| `SubI r x #i` | `r <- x - #i` |
| `Mult r x y` | `r <- x * y` |
| `Div r x y` | `r <- x / y` |
| `Eq r x y` | `r <- x == y` |
| `Lt r x y` | `r <- x < y` |
| `Or r x y` | `r <- x || y` |
| `And r x y` | `r <- x && y` |
| `Not r x` | `r <- !x` |
||
| `B addr` | `pc <- addr` |
| `BT addr` | `if (r == 1) then pc <- addr` |
| `BF addr` | `if (r == 0) then pc <- addr` |
| `Ret` | `pc <- lr` |
| `SysCall` | End execution |
||
| `Print r` | Print value of register `r` to output |
| `PrintLn` | Print a newline to output |
