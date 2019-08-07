# Bfl (baffle)
A brainfuck interpreter/compiler

## Usage
### Run a brainfuck program
```bash
bf run <filename>.bf
```
### Compile a brainfuck program to C
```bash
bf compile <filename>.bf <target>.c
```
You can then compile the target using your favorite C compiler and execute the binary:
```bash
gcc <target>.c
./a.out
```