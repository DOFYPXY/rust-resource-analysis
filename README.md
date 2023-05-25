# Rust Resource Analysis Benchmark

## Structure

- `src/`: the Rust source.
- `llbc/`: LLBC files compiled by Charon
- `fstar/`: Fstar implementations translated by Aeneas
- `ocaml/`: Ocaml implementation compiled by `fstar.exe --codegen`
- `raml/`: manually modified codes for RaML

## Commands

```
../charon/bin/charon --input src/no_nested_borrows.rs --dest llbc --crate no_nested_borrows  
../aeneas/bin/aeneas.exe -backend fstar llbc/no_nested_borrow.llbc -no-split-files -dest fstar 
fstar.exe --codegen OCaml  --odir ocaml fstar/NoNestedBorrow.fst   
```