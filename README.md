Interpreter
===========

A fast register bytecode interpreter and compiler for a small subset of the Euphoria programming language.

The interpreter back-end is written in C.  It reads instructions from a file 
or standard input, then executes them using a simple register virtual machine.
To compile the interpreter, run `make` (or using LLVM `CC=clang make -B`).

The compiler front-end is written in Euphoria.  It reads a source code file,
parses into tokens, building an abstract syntax tree (AST) representation of
the program.  The compiler takes the AST and generates VM instructions, 
printing them on standard output.  For example:

	interpreter$ exu compile.ex test/primes.ex
	4B400401        load   r4, 19264
	004C0402        loadhi r4, 76
	00010501        load   r5, 1
	00000101        load   r1, 0
	0E05040A        jle    r4, r5, 14
	00030201        load   r2, 3
	00010301        load   r3, 1
	02050505        add    r5, r5, 2
	02020606        mul    r6, r2, r2
	0606050A        jle    r5, r6, 6
	0500030B        je     r3, r0, 5
	02050707        div    r7, r5, r2
	02070606        mul    r6, r7, r2
	0506030F        neq    r3, r6, r5
	02020205        add    r2, r2, 2
	FFF8000D        jmp    -8
	0100030B        je     r3, r0, 1
	01010105        add    r1, r1, 1
	FFF1000D        jmp    -15
	00000114        qprint r1
	00000000        end    

To execute the instructions, pipe them through the interpreter:

	interpreter$ exu compile.ex test/primes.ex | ./interpret 
	348842

Or save the instructions to a file, then interpret them:

	interpreter$ exu compile.ex test/primes.ex > primes.txt
	interpreter$ ./interpret primes.txt
	348842

To time the performance of execution, use `time ./interpret` or `perf stat ./interpret`.

Instruction format
------------------

The instructions are encoded as 32-bit integers, with two encodings:

	[31...24] [23...16] [15....8] [7.....0]
	    C         B         A      Operand

	[31.............16] [15....8] [7.....0]
	         D              A      Operand

The operands are listed in `opcodes.e`.  The general operation for instructions
is `reg[A] = reg[B] op reg[C]` or `jump by C if reg[A] op reg[B]`.  Loads and 
unconditional jumps use a 16-bit sign-extended `D` value in the place of `B` 
and `C`.  All jumps are relative number of instructions (not bytes) 
using sign-extended C or D.  The virtual machine has 256 registers, with 
`reg[0]` set to constant zero.

The interpreter uses a jump table for fast decoding of opcode handler addresses.
The latest GCC and LLVM generate really nice code for decoding the instructions.
The whole instruction is loaded in a machine register, and the Operand byte is 
extended from the `L` byte using `movzx` instruction to index into the jump 
table.  The A byte is extended from the machine register `H` byte.  The 
register is then shifted arithmetic right `sar` 16 bits to give D, or then 
can use `movzx` twice to get of B and C from the `L` and `H` registers.

GCC seems to generate an extra branch for each operand handler due to 
common subexpression elimination, but this doesn't seem to hurt performance.
LLVM generates one branch to the next opcode at the end of each opcode handler.

Limitations
-----------
 * The compiler only handles integers, and doesn't handle integer overflow.
   I had started adding an overflow flag check `INTO`, but it is currently disabled.
 * The compiler will fail badly if it runs out of registers.
 * Relative jump range cannot exceed signed byte.


