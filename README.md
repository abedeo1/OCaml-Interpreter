# OCaml Stack-Based Bytecode Interpreter

## Overview 

This project implements a stack based bytecode interpreter in OCaml. The interpreter reads a series of bytecode commands from an input file and executes them in order, simulating a basic virtual machine. Output is written to a separate output file.


## Core Function

The interpreter supports various stack operations including:

- Arithmetic: add, sub, mul, div, rem, neg  
- Boolean logic: and, or, not, lessThan, equal, if  
- Stack operations: push, pop, swap  
- String manipulation: cat, toString  
- Bindings: bind  
- IO: println  
- Program control: quit

Each command operates on a stack of values, which can be:


type stackValue =
  | BOOL of bool
  | INT of int
  | ERROR
  | STRING of string
  | NAME of string
  | UNIT



## Input Format  

Input files must contain one command per line. Supported inputs include:

- Integers, e.g. push 5  
- Booleans, e.g. push :true:  
- Strings, e.g. push "hello"  
- Special values, e.g. :unit:, :error:  
- Names (for binding), e.g. push x


Example input 1:
push 10
push 2
mul
push 3
sub
println
quit

Example output 1:
17

Example input 2:
push 5
push x
bind
push x
push 3
add
println
quit

Example output 2:
8


## Testing  

You can test the interpreter by creating test input files locally and uncommenting the testcases at the bottom of interpreter.ml.


## To run locally

```bash
ocamlc -o interpreter interpreter.ml
./interpreter
```
