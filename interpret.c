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



unsigned int *base = NULL;

//#define TRACE
#ifndef TRACE

#define trace(bc,regs) do{}while(0)

#else
int trace(unsigned int *bc, int regs[])
{
    int i = *bc;
    int a = (i >> 8) & 0xff;
    int b = (i >> 16) & 0xff;
    int c = (i >> 24) & 0xff;
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
        &&op_jl,
        &&op_jle,
        &&op_je,
        &&op_jne,
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
    };
    int regs[256];

    long i, j;
    int x;
    short k;
    const void *op;

#define A (j)
#define B (k & 0xff)
#define C ((k & 0xff00) >> 8)
#define D (k)

#define RA regs[A]
#define RB regs[B]
#define RC regs[C]

#define NEXT do { \
    trace(bc, regs); \
    i = *bc++; \
    op = jt[i&0xff]; \
    j = (i & 0xff00) >> 8; \
    k = i >> 16; \
    goto *op; \
    } while (0)

#define NEXTA(expr) do { \
    i = *bc++; \
    op = jt[i&0xff]; \
    x = (expr); \
    k = i >> 16; \
    RA = x; \
    trace(bc-1, regs); \
    j = (i & 0xff00) >> 8; \
    goto *op; \
    } while (0)

    regs[0] = 0; // r0 is always zero
    NEXT;
op_load:
    NEXTA((short)D);
op_loadhi:
    RA ^= (D << 16);
    NEXT;
op_mov:
    NEXTA(RB);
op_add:
    NEXTA(RB + RC);
op_addu8:
    NEXTA(RB + (unsigned char)C);
op_mul:
    NEXTA(RB * RC);
op_div:
    NEXTA(RB / RC);
op_rem:
    NEXTA(RB % RC);
op_jl:
    if (RA < RB)
        bc += (char)C;
    NEXT;
op_jle:
    if (RA <= RB)
        bc += (char)C;
    NEXT;
op_je:
    if (RA == RB)
        bc += (char)C;
    NEXT;
op_jne:
    if (RA != RB)
        bc += (char)C;
    NEXT;
op_jmp:
    bc += (short)D;
    NEXT;
op_eq:
    NEXTA(RB == RC);
op_neq:
    NEXTA(RB != RC);
op_lt:
    NEXTA(RB < RC);
op_gte:
    NEXTA(RB >= RC);
op_lte:
    NEXTA(RB <= RC);
op_gt:
    NEXTA(RB > RC);
op_qprint:
    printf("%d\n", RA);
    NEXT;
op_sub:
    NEXTA(RB - RC);
op_subu8:
    NEXTA(RB - (unsigned char)C);
op_not:
    NEXTA(!RB);
op_neg:
    NEXTA(-RB);
op_end:
    return;
}

unsigned int *read_bytecode(FILE *f)
{
    unsigned int len = 0, size = 0;
    unsigned int *bc = NULL;
    
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
        bc[len++] = strtoul(line, NULL, 16);
    }
    return bc;    
}


int main(int argc, char *argv[])
{
    unsigned int *bc = NULL;
    
    if (argc > 1) {
        FILE *f;
        f = fopen(argv[1], "r");
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
