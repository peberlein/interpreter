/*
 *  interpret.c
 *
 * Copyright (c) 2014 Pete Eberlein
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>


enum {
#define global
#define constant
#include "opcodes.e"
#undef global
#undef constant
};

// adjustable parameters for tuning performance
//#define MULTI_BRANCH
//#define SWAP_ABC
//#define BRANCH_CMOV


unsigned int *base = NULL;

//#define TRACE
#ifndef TRACE

#define trace(bc,regs) do{}while(0)

#else
int trace(unsigned int *bc, int regs[])
{
    int i = *bc;
#ifdef SWAP_ABC
    int a = (i >> 8) & 0xff;
    int b = (i >> 16) & 0xff;
    int c = (i >> 24) & 0xff;
#else
    int a = (i >> 24) & 0xff;
    int b = (i >> 8) & 0xff;
    int c = (i >> 16) & 0xff;
#endif
    int d = (i >> 16);

    printf("%d: ", (int)(bc - base));

    switch (i & 0xff) {
        case LOAD:
            printf("load r%d, %d\n", a, d);
            break;
        case LOADHI:
            printf("loadhi r%d, %d\n", a, d);
            break;
        case MOV:
            printf("mov r%d, r%d (%d)\n", a, b, regs[b]);
            break;
        case ADD:
            printf("add r%d, r%d, r%d (%d %d)\n", a, b, c, regs[b], regs[c]);
            break;
        case ADDU8:
            printf("addu8 r%d, r%d, %d (%d)\n", a, b, c, regs[b]);
            break;
        case MUL:
            printf("mul r%d, r%d, r%d (%d %d)\n", a, b, c, regs[b], regs[c]);
            break;
        case DIV:
            printf("div r%d, r%d, r%d (%d %d)\n", a, b, c, regs[b], regs[c]);
            break;
        case REM:
            printf("rem r%d, r%d, r%d (%d %d)\n", a, b, c, regs[b], regs[c]);
            break;
        case JL:
            printf("jl r%d, r%d, %d (%d %d)\n", a, b, (char)c, regs[a], regs[b]);
            break;
        case JLE:
            printf("jle r%d, r%d, %d (%d %d)\n", a, b, (char)c, regs[a], regs[b]);
            break;
        case JE:
            printf("je r%d, r%d, %d (%d %d)\n", a, b, (char)c, regs[a], regs[b]);
            break;
        case JNE:
            printf("jne r%d, r%d, %d (%d %d)\n", a, b, (char)c, regs[a], regs[b]);
            break;
        case JMP:
            printf("jmp %d\n", d);
            break;
        case EQ:
            printf("eq r%d, r%d, r%d (%d %d)\n", a, b, c, regs[b], regs[c]);
            break;
        case NEQ:
            printf("neq r%d, r%d, r%d (%d %d)\n", a, b, c, regs[b], regs[c]);
            break;
        case QPRINT:
            printf("? r%d\n", a);
            break;
        case END:
            printf("end\n");
            break;

        default:
            printf("invalid opcode %d\n", i & 0xff);
            exit(1);
            break;
    }
    return i & 0xff;
}
#endif

//#define INTO asm("jno .+4\n int $4\n" ::: "cc", "memory")
#define INTO

static void interpret(unsigned int *bc)
{
    static const void * const jt[] = {
        &&op_end,
        &&op_load,
        &&op_loadhi,
        &&op_mov,
        &&op_add,
        &&op_addu8,
        &&op_mul,
        &&op_div,
        &&op_rem,
#ifdef MULTI_BRANCH
        &&op_jl,
        &&op_jl_1, &&op_jl_2, &&op_jl_3, &&op_jl_4, &&op_jl_5, &&op_jl_6, &&op_jl_7,
        &&op_jle,
        &&op_jle1, &&op_jle2, &&op_jle3, &&op_jle4, &&op_jle5, &&op_jle6, &&op_jle7,
        &&op_je,
        &&op_je_1, &&op_je_2, &&op_je_3, &&op_je_4, &&op_je_5, &&op_je_6, &&op_je_7,
        &&op_jne,
        &&op_jne1, &&op_jne2, &&op_jne3, &&op_jne4, &&op_jne5, &&op_jne6, &&op_jne7,
#else
        &&op_jl, &&op_jl, &&op_jl, &&op_jl, &&op_jl, &&op_jl, &&op_jl, &&op_jl,
        &&op_jle, &&op_jle, &&op_jle, &&op_jle, &&op_jle, &&op_jle, &&op_jle, &&op_jle,
        &&op_je, &&op_je, &&op_je, &&op_je, &&op_je, &&op_je, &&op_je, &&op_je,
        &&op_jne, &&op_jne, &&op_jne, &&op_jne, &&op_jne, &&op_jne, &&op_jne, &&op_jne,
#endif
        &&op_jmp,
        &&op_eq,
        &&op_neq,
        &&op_lt,
        &&op_gte,
        &&op_lte,
        &&op_gt,
        &&op_qprint,
        &&op_sub,
        &&op_subu8,
        &&op_not,
        &&op_neg,
        &&op_and,
        &&op_or,
        &&op_xor,
    };
    int regs[256];

    long i = 0, j = 0, k = 0;
    int x = 0;
    //short k;
    const void *op;

#ifdef SWAP_ABC
#define B (j)
#define C (k & 0xff)
#define A ((k & 0xff00) >> 8)
#define D (k)
#else
#define A (j)
#define B (k & 0xff)
#define C ((k & 0xff00) >> 8)
#define D (k)
#endif

#define RA regs[A]
#define RB regs[B]
#define RC regs[C]


#define NEXT(st1,st2) do { \
    { st1; }  \
    trace(bc, regs); \
    i = *bc++; \
    op = jt[i&0xff]; \
    k = i >> 16; \
    { st2; }  \
    j = (i & 0xff00) >> 8; \
    goto *op; \
    } while (0)

#ifdef SWAP_ABC
#define NEXTA(expr) do { \
    x = (expr); \
    i = *bc++; \
    trace(bc-1, regs); \
    op = jt[i&0xff]; \
    j = (i & 0xff00) >> 8; \
    RA = x; \
    k = i >> 16; \
    goto *op; \
    } while (0)
#else
#define NEXTA(expr) NEXT(x = (expr), RA = x)
#endif

#define NEXTB(expr) NEXT(x = (expr), regs[j] = x)


#ifdef BRANCH_CMOV
#define BRANCH(op) do { \
    long c = (char)C;   \
    asm("leaq (%0,%1,4),%1\n" \
        "cmp %3, %2\n"        \
        "cmov" op " %1, %0\n" \
        :"+r"(bc),            \
         "+r"(c)              \
        :"r"(RA),             \
         "m"(RB));            \
    NEXT(,);                  \
    } while (0)
#define BRANCH_L  BRANCH("l")
#define BRANCH_LE BRANCH("le")
#define BRANCH_E  BRANCH("e")
#define BRANCH_NE BRANCH("ne")

#else

#define BRANCH(expr) do { \
    i = *bc; \
    op = jt[i&0xff]; \
    if (expr) { bc += (char)C; NEXT(,); } \
    bc++; \
    k = i >> 16; \
    j = (i & 0xff00) >> 8; \
    goto *op; \
    } while (0)
#define BRANCH_L  BRANCH(RA < RB)
#define BRANCH_LE BRANCH(RA <= RB)
#define BRANCH_E  BRANCH(RA == RB)
#define BRANCH_NE BRANCH(RA != RB)
#endif

    regs[0] = 0; // r0 is always zero
    NEXT(,);
op_load: NEXTB((short)D);
op_loadhi: NEXTB(regs[j] ^ (D << 16));
op_mov: NEXTA(RB);
op_add: NEXTA(RB + RC);
op_addu8: NEXTA(RB + (unsigned char)C);
op_mul: NEXTA(RB * RC);
op_div: NEXTA(RB / RC);
op_rem: NEXTA(RB % RC);
op_jl:  BRANCH_L;
op_jle: BRANCH_LE;
op_je:  BRANCH_E;
op_jne: BRANCH_NE;
op_jmp: bc += (short)D; NEXT(,);
op_eq:  NEXTA(RB == RC);
op_neq: NEXTA(RB != RC);
op_lt:  NEXTA(RB < RC);
op_gte: NEXTA(RB >= RC);
op_lte: NEXTA(RB <= RC);
op_gt:  NEXTA(RB > RC);
op_qprint:  printf("%d\n", regs[j]); NEXT(,);
op_sub: NEXTA(RB - RC);
op_subu8: NEXTA(RB - (unsigned char)C);
op_not: NEXTA(!RB);
op_neg: NEXTA(-RB);
op_and: NEXTA(RB && RC);
op_or:  NEXTA(RB || RC);
op_xor: NEXTA((!RB) ^ (!RC));

#ifdef MULTI_BRANCH
/* Yes, these are duplicated, to help branch prediction */
op_jl_1: BRANCH_L;
op_jl_2: BRANCH_L;
op_jl_3: BRANCH_L;
op_jl_4: BRANCH_L;
op_jl_5: BRANCH_L;
op_jl_6: BRANCH_L;
op_jl_7: BRANCH_L;
op_jle1: BRANCH_LE;
op_jle2: BRANCH_LE;
op_jle3: BRANCH_LE;
op_jle4: BRANCH_LE;
op_jle5: BRANCH_LE;
op_jle6: BRANCH_LE;
op_jle7: BRANCH_LE;
op_je_1: BRANCH_E;
op_je_2: BRANCH_E;
op_je_3: BRANCH_E;
op_je_4: BRANCH_E;
op_je_5: BRANCH_E;
op_je_6: BRANCH_E;
op_je_7: BRANCH_E;
op_jne1: BRANCH_NE;
op_jne2: BRANCH_NE;
op_jne3: BRANCH_NE;
op_jne4: BRANCH_NE;
op_jne5: BRANCH_NE;
op_jne6: BRANCH_NE;
op_jne7: BRANCH_NE;
#endif

op_end:
    return;
}

unsigned int *read_bytecode(FILE *f)
{
    unsigned int len = 0, size = 0;
    unsigned int *bc = NULL;
    unsigned int x, op;
    
    while (!feof(f)) {
        char line[256];
        if (!fgets(line, sizeof line, f))
            break;
        //printf("%s", line);
        if (len >= size) {
            size += 1024;
            bc = realloc(bc, size * sizeof(*bc));
            if (!bc) {
                fputs("out of memory\n", stderr);
                exit(1);
            }
            base = bc;
        }
        x = strtoul(line, NULL, 16);
        op = x & 0xff;
        if (op == END && (x >> 16) != OP_VER) {
            fputs("wrong opcode version, please compile again\n", stderr);
            exit(1);
        }
#ifdef SWAP_ABC
        if (op != END && op != JMP && op != LOAD && op != LOADHI && op != QPRINT) {
            x = op | ((x & 0xffff0000) >> 8) | ((x & 0xff00) << 16);
        }
#endif
        bc[len++] = x;
    }
    return bc;    
}


int main(int argc, char *argv[])
{
    unsigned int *bc = NULL;
    
    if (argc > 1) {
        FILE *f = fopen(argv[1], "r");
        if (!f) {
            perror(argv[1]);
            exit(1);
        }
        bc = read_bytecode(f);
        fclose(f);
    } else {
        bc = read_bytecode(stdin);
    }
    
    if (bc) {
        interpret(bc);
        free(bc);
    }

    return 0;
}
