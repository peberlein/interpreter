-- compile.ex
-- 
-- Copyright (c) 2014 Pete Eberlein
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.


without warning

include opcodes.e

constant argv = command_line(), filename = argv[3]

sequence types
types = {
  "object",
  "integer",
  "atom",
  "sequence"
}

atom f, idx, tok_idx
object line
sequence file

idx = 1
tok_idx = 1

f = open(filename, "rb")
line = gets(f)
file = {}
while sequence(line) do
  file &= line
  line = gets(f)
end while


procedure error(sequence msg)
  integer line, start

  line = 1
  start = 1
  
  for i = 1 to tok_idx do
    if file[i] = '\n' then
      line += 1
      start = i+1
    end if
  end for
  for i = tok_idx to length(file) do
    if file[i] = '\n' then
      --? {start, idx, tok_idx}
      printf(2, "%s:%d\n%s\n%s%s\n", {
        filename,
        line, 
        msg,
        file[start..i],
        repeat(' ', tok_idx - start) & '^'})
      abort(1)
    end if
  end for
  puts(1, "unexpected end of file\n")
  abort(1)
end procedure


-- ast node types
constant
  VAR_DECL = 256,
  ASSIGN = 257,
  FUNC = 258,     -- {FUNC, "name", [args,]...}
  VARIABLE = 259,
  SUBSCRIPT = 260,
  SLICE = 261,

  CAT = 263,



  SEQ = 267,    -- {SEQ, expr...}
  INTEGER = 268,
  WHILE = 269,  -- {WHILE, expr, stmts...}
  IF = 270,     -- {IF, expr, {stmts...}, [expr, {elsif-stmts...},]... [{else-stmts...}]}
  FOR = 271,    -- {FOR, name, expr, expr, by, stmts...}
  PROC = 272,
  FUNC_DECL = 273,
  PROC_DECL = 274,
  ELSIF = 275,
  ELSE = 276,
  ADDTO = 277,
  SUBTO = 278,
  MULTO = 279,
  DIVTO = 280,
  CATTO = 281,
  STRING = 282,

  GLOBAL = 0

constant ast_names = { "var_decl", "assign", "func", "variable",
"subscript", "slice", "neg", "not", "mul", "div", "add", "sub",
"cat", "lt", "gt", "lte", "gte", "eq", "neq", "and", "or", "xor",
"seq", "integer", "while", "if", "for", "proc", "func_decl",
"proc_decl", "elsif", "else", "qprint", "addto", "subto", 
"multo", "divto", "catto", "string" }



procedure skip_whitespace()
  integer c
  if idx > length(file) then return end if
  c = file[idx]
  while find(c, "\t\r\n -") do
    if c = '-' then
      if file[idx+1] != '-' then 
        exit 
      end if
      -- skip comment
      while not find(c, "\r\n") do
        idx += 1
        if idx > length(file) then return end if
        c = file[idx]
      end while
    end if
    idx += 1
    if idx > length(file) then return end if
    c = file[idx]
  end while
end procedure
  
function isalpha(integer c)
  return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') 
end function

function isnum(integer c)
  return (c >= '0' and c <= '9')
end function

function isalphanum(integer c)
  return isalpha(c) or isnum(c) or c = '_'
end function


sequence tok
tok = ""

function token(sequence try)
  if length(tok) = 0 then
    skip_whitespace()
    if idx > length(file) then return 0 end if
    tok_idx = idx
    tok = {file[idx]}
    idx += 1
    if isalpha(tok[1]) then
      while idx <= length(file) and isalphanum(file[idx]) do
        tok &= file[idx]
        idx += 1
      end while
    elsif isnum(tok[1]) then
      while idx <= length(file) and isnum(file[idx]) do
        tok &= file[idx]
        idx += 1
      end while
    elsif idx <= length(file) and file[idx] = '=' and find(tok[1], "<>+-*/&!") then
      tok &= '='
      idx += 1
    end if
    --printf(1, "token: %s\n", {tok})
  end if
  if equal(tok, try) then
    tok = ""
    return 1
  end if
  return 0
end function

procedure expect(sequence try)
  if not token(try) then
    error("expected " & try)
  end if
end procedure

function identifier()
  if length(tok) = 0 then
    if token("") then end if
  end if
  return isalpha(tok[1])
end function

function get_token()
  sequence result
  result = tok
  tok = ""
  return result
end function

function string()
  sequence s
  s = ""
  while file[idx] != '"' do
    if file[idx] = '\n' then
      error("unterminated string literal")
    end if
    if file[idx] = '\\' then
      idx += 1
      if idx > length(file) then
      elsif file[idx] = 't' then
        s &= '\t'
      elsif file[idx] = 'r' then
        s &= '\r'
      elsif file[idx] = 'n' then
        s &= '\n'        
      elsif file[idx] = '\\' then
        s &= '\\'
      else
        error("unknown escape character")
      end if
    else
      s &= file[idx]
    end if
    idx += 1
    if idx > length(file) then
      error("unexpected end of file")
    end if
  end while
  idx += 1
  return s
end function

function to_integer(sequence s)
  atom x
  x = 0
  for i = 1 to length(s) do
    x = x * 10 + s[i] - '0'
  end for
  return x
end function


constant 
precedence = {
  {"and", "or", "xor"},
  {"<=", ">=", "<", ">", "=", "!="},
  {"&"},
  {"+", "-"},
  {"*", "/"}
}, 
precedence_ast = {
  {AND, OR, XOR},
  {LTE, GTE, LT, GT, EQ, NEQ},
  {CAT},
  {ADD, SUB},
  {MUL, DIV}
}


function expr(integer depth)
  sequence e
  integer ok
  
  if depth <= length(precedence) then
    e = expr(depth + 1)
    ok = 1
    while ok do
      ok = 0
      for i = 1 to length(precedence[depth]) do
        if token(precedence[depth][i]) then
          e = {precedence_ast[depth][i], e, expr(depth+1)}
          ok = 1
        end if
      end for
    end while

  else
    if identifier() then
      e = {VARIABLE, get_token()}
      if token("[") then
        expect("]")

      elsif token("(") then
        -- function call
        if not token(")") then
          e[1] = FUNC
          ok = 1
          while ok do
            e = append(e, expr(1))
            ok = token(",")
          end while
          expect(")")
        end if
      end if

    elsif token("(") then
      e = expr(1)
      expect(")")

    elsif token("{") then
      e = {SEQ}
      if not token("}") then
        e = append(e, expr(1))
        while token(",") do
          e = append(e, expr(1))
        end while
        expect("}")
      end if

    elsif isnum(tok[1]) then
      e = {INTEGER, to_integer(get_token())}
    elsif token("-") then
      e = {NEG, expr(depth)}
    elsif token("+") then
      e = expr(depth)
    elsif token("not") then
      e = {NOT, expr(depth)}
    elsif token("\"") then
      e = {STRING, string()}
    else
      error("expected expression")
    end if
  end if
  
  --printf(1, "expr(%d): ", {depth})
  --? e
  return e
end function




function variable_declaration()
  sequence result
  result = {VAR_DECL, get_token()}
  while identifier() do
    result = append(result, get_token())
    if not token(",") then
      return result
    end if
  end while
  error("expected variable name")
end function

function assignment_or_procedure_call()
  sequence result

  if identifier() then
    result = get_token()
    if token("=") then
      return {ASSIGN, result, expr(1)}
    elsif token("+=") then
      return {ADDTO, result, expr(1)}
    elsif token("-=") then
      return {SUBTO, result, expr(1)}
    elsif token("*=") then
      return {MULTO, result, expr(1)}
    elsif token("/=") then
      return {DIVTO, result, expr(1)}
    elsif token("&=") then
      return {CATTO, result, expr(1)}
    elsif token("(") then
      result = {PROC, result}
      if not token(")") then
        result = append(result, expr(1))
        while token(",") do
          result = append(result, expr(1))
        end while
        expect(")")
      end if
      return result
    end if
  end if
  return ""
end function


function parse(integer mode)
  sequence ast, s
  ast = {}
  while idx < length(file) do
    if mode = IF and token("elsif") then
        tok = "elsif"
        return ast
    elsif mode = IF and token("else") then
        tok = "else"
        return ast
    elsif mode != GLOBAL and token("end") then
      return ast
    end if

    s = {}
    if token("while") then
      s = {WHILE, expr(1)}
      expect("do")
      s &= parse(WHILE)
      expect("while")

    elsif token("if") then
      s = {IF, expr(1)}
      expect("then")
      s = append(s, parse(IF))
      while token("elsif") do
        s = append(s, expr(1))
        s = append(s, parse(IF))
      end while
      if token("else") then
        s = append(s, parse(ELSE))
      end if
      expect("if")

    elsif token("?") then
      s = {QPRINT, expr(1)}
      
    elsif token("without") then
      if token("type_check") then
      elsif token("warning") then
      else
        error("unknown without option")
      end if
    
    elsif identifier() then
      if find(tok, types) then
        -- variable declaration
        s = variable_declaration()
      else
        s = assignment_or_procedure_call()
      end if
      if length(s) = 0 then
        error("expected statement")
      end if
    end if
    --? s
    ast = append(ast, s)
  end while
  return ast
end function



sequence regs
regs = repeat(0, 255)
-- regs 0 is reserved for zero

function asm(integer op, integer a, integer b, integer c)
  return op + a * #100 + b * #10000 + c * #1000000
end function

function asm2(integer op, integer a, integer d)
  return op + a * #100 + d * #10000
end function

procedure show_regs()
  for i = 1 to length(regs) do
    if sequence(regs[i]) then
      printf(1, "r%d %s\n", {i, regs[i]})
    elsif regs[i] != 0 then
      printf(1, "r%d <temp>\n", {i})
    end if
  end for
end procedure


-- compile_expr may return a temporary register 
-- in which case we need to free it when we're done
-- (registers allocated to a variable will not be freed)
procedure free_temporary_reg(integer r)
  if integer(regs[r]) then
    regs[r] = 0
  end if
end procedure

constant ALLOC_REG = -1

-- compile an expression into register d returning {bytecode}
-- if d=-1, a temporary register, returning {register, {bytecode}}
function compile_expr(sequence e, integer a)
  integer b, op
  sequence r1, r2

  op = e[1]
  if a = ALLOC_REG then
    if op = VARIABLE then
      return {find(e[2], regs), {}}
    elsif op = INTEGER and e[2] = 0 then
      return {0, {}} -- r0, zero
    end if
    -- find a temporary reg
    a = find(0, regs)
    regs[a] = -1
    return {a, compile_expr(e, a)}
  end if

  if op = VARIABLE then
    b = find(e[2], regs)
    return asm(MOV, a, b, 0)

  elsif op = INTEGER then
    if e[2] >= -#8000 and e[2] < #8000 then
      return asm2(LOAD, a, e[2])
    end if
    if and_bits(e[2], #8000) then
      -- LOAD sign-extends, so need to invert high bits
      return asm2(LOAD, a, and_bits(e[2], #FFFF)) & 
        asm2(LOADHI, a, xor_bits(floor(e[2] / #10000), #FFFF))
    else
      return asm2(LOAD, a, and_bits(e[2], #FFFF)) & 
        asm2(LOADHI, a, floor(e[2] / #10000))
    end if

  elsif op = ADD then
    if e[2][1] = INTEGER and e[2][2] >= 0 and e[2][2] <= 255 then
      r2 = compile_expr(e[3], ALLOC_REG)
      free_temporary_reg(r2[1])
      return r2[2] & asm(ADDU8, a, r2[1], e[2][2])
    elsif e[3][1] = INTEGER and e[3][2] >= 0 and e[3][2] <= 255 then
      r1 = compile_expr(e[2], ALLOC_REG)
      free_temporary_reg(r1[1])
      return r1[2] & asm(ADDU8, a, r1[1], e[3][2])
    end if
    r1 = compile_expr(e[2], ALLOC_REG)
    r2 = compile_expr(e[3], ALLOC_REG)
    free_temporary_reg(r1[1])
    free_temporary_reg(r2[1])
    return r1[2] & r2[2] & asm(op, a, r1[1], r2[1])

  elsif op = EQ or op = NEQ or op = LT or op = LTE
     or op = GT or op = GTE or op = MUL 
     or op = AND or op = OR or op = XOR
  then
    r1 = compile_expr(e[2], ALLOC_REG)
    r2 = compile_expr(e[3], ALLOC_REG)
    free_temporary_reg(r1[1])
    free_temporary_reg(r2[1])
    return r1[2] & r2[2] & asm(op, a, r1[1], r2[1])
    
  elsif op = NOT or op = NEG then
    r1 = compile_expr(e[2], ALLOC_REG)
    free_temporary_reg(r1[1])
    return r1[2] & asm(op, a, r1[1], 0)

  elsif e[1] = FUNC then
    if equal(e[2], "floor") and e[3][1] = DIV then
      e = e[3]
      r1 = compile_expr(e[2], ALLOC_REG)
      r2 = compile_expr(e[3], ALLOC_REG)
      free_temporary_reg(r1[1])
      free_temporary_reg(r2[1])
      return r1[2] & r2[2] & asm(DIV, a, r1[1], r2[1])

    else
      puts(2, "floor not implemented\n")
    end if
  else
    printf(2, "expr %d %s\n", {e[1], ast_names[e[1]]})
  end if

  return {}
end function

-- simplify expression e using de morgan's laws
function de_morgans_laws(sequence e)
  integer op
  if e[1] = NOT then
    op = e[2][1]
    if op = NOT then
      return de_morgans_laws(e[2][2])
    elsif op = AND then
      return {OR, de_morgans_laws({NOT, e[2][2]}),
                  de_morgans_laws({NOT, e[2][3]})}
    elsif op = OR then
      return {AND, de_morgans_laws({NOT, e[2][2]}),
                   de_morgans_laws({NOT, e[2][3]})}
    elsif op = EQ or op = NEQ or op = LT
       or op = GTE or op = GT or op = LTE
    then
      e = e[2]
      -- swap EQ with NEQ, LT with GTE, GT with LTE
      e[1] = xor_bits(e[1], 1)
      ? e
    end if
  end if
  return e
end function

-- compile expression e: 
-- when TRUE, fall thru  (common idiom for while/if/elsif)
-- when FALSE, jump by d instructions
function compile_jmp_expr(sequence e, integer d)
  integer a, b, op
  sequence bc, ra, rb

  e = de_morgans_laws(e)
  op = e[1]
  bc = {}

  if op = LT or op = GT or op = LTE 
  or op = GTE or op = EQ or op = NEQ
  then
    ra = compile_expr(e[2], ALLOC_REG)
    a = ra[1]
    bc &= ra[2]
    rb = compile_expr(e[3], ALLOC_REG)
    b = rb[1]
    bc &= rb[2]
    
    if d < 0 then
      d -= length(ra[2]) + length(rb[2]) + 1
    end if

    if op = LT then
      bc &= asm(JLE, b, a, d)
    elsif op = GT then
      bc &= asm(JLE, a, b, d)
    elsif op = LTE then
      bc &= asm(JL, b, a, d)
    elsif op = GTE then
      bc &= asm(JL, a, b, d)
    elsif op = EQ then
      bc &= asm(JNE, a, b, d)
    elsif op = NEQ then
      bc &= asm(JE, a, b, d)
    end if

    free_temporary_reg(a)
    free_temporary_reg(b)

  elsif op = OR then
    -- jmp over next st if e[2] is true (short circuit)
    -- jmp if e[3] is false
    rb = compile_jmp_expr(e[3], d)
    ra = compile_jmp_expr({NOT, e[2]}, length(rb))
    -- FIXME this adjustment won't work
    if d < 0 then
      rb[$] -= length(ra) * #1000000
    end if
    bc &= ra
    bc &= rb

  elsif op = AND then
    -- jmp if e[2] is false (short circuit)
    -- jmp if e[3] is false
    if d < 0 then
      ra = compile_jmp_expr(e[2], d)
      d -= length(ra)
      rb = compile_jmp_expr(e[3], d)
    else
      rb = compile_jmp_expr(e[3], d)
      d += length(rb)
      ra = compile_jmp_expr(e[2], d)
    end if
    bc &= ra
    bc &= rb

  else
    -- compare it to zero
    if op = NOT then
      e = e[2]
      op = JNE
    else
      op = JE
    end if
    ra = compile_expr(e, ALLOC_REG)
    free_temporary_reg(ra[1])
    bc &= ra[2]
    if d < 0 then
      d -= length(ra[2]) + 1
    end if
    bc &= asm(op, ra[1], 0, d)

  end if
  return bc
end function


function compile(sequence ast)
  sequence s, bc, st, fix
  integer reg
  
  bc = {}

  for i = 1 to length(ast) do
    s = ast[i]
    if s[1] = VAR_DECL then
      -- allocate some registers for those variables
      for j = 3 to length(s) do
        reg = find(0, regs)
        regs[reg] = s[j]
      end for

    elsif s[1] = ASSIGN then
      reg = find(s[2], regs)
      bc &= compile_expr(s[3], reg)

    elsif s[1] = WHILE then
      st = compile(s[3..$])
      st = compile_jmp_expr(s[2], length(st)+1) & st
      st &= asm2(JMP, 0, -length(st)-1)
      bc &= st
      
    elsif s[1] = IF then
      fix = {}
      for j = 2 to length(s) by 2 do
        if j != length(s) then
          -- 'if' or 'elsif'
          st = compile(s[j+1])
          if j+1 < length(s) then
            -- before an 'else' or 'elsif',
            -- insert placeholder for jmp to 'end if'
            st &= 0
          end if
          bc &= compile_jmp_expr(s[j], length(st))
          bc &= st
          if j+1 < length(s) then
            fix &= length(bc)
          end if
        else
          -- 'else'
          bc &= compile(s[j])
        end if
      end for
      for j = 1 to length(fix) do
        bc[fix[j]] = asm2(JMP, 0, length(bc) - fix[j])
      end for
      
    elsif s[1] = QPRINT then
      st = compile_expr(s[2], ALLOC_REG)
      free_temporary_reg(st[1])
      bc &= st[2] & asm(QPRINT, st[1], 0, 0)

    else
      printf(2, "stmt %d %s\n", {s[1], ast_names[s[1]]})

    end if

  end for
  
  return bc
end function

procedure disasm(sequence bc)
  integer op, a, b, c, d
  for i = 1 to length(bc) do
    op = and_bits(bc[i], #FF)
    a = and_bits(floor(bc[i] / #100), #FF)
    b = and_bits(floor(bc[i] / #10000), #FF)
    c = and_bits(floor(bc[i] / #1000000), #FF)
    d = and_bits(floor(bc[i] / #10000), #FFFF)
    printf(1, "%08x\t", {bc[i]})
    if op = LOAD then
      printf(1, "load   r%d, %d\n", {a, d - 2 * and_bits(d, #8000)})
    elsif op = LOADHI then
      printf(1, "loadhi r%d, %d\n", {a, d})
    elsif op = MOV then
      printf(1, "mov    r%d, r%d\n", {a, b})
    elsif op = ADD then
      printf(1, "add    r%d, r%d, r%d\n", {a, b, c})
    elsif op = ADDU8 then
      printf(1, "add    r%d, r%d, %d\n", {a, b, c})
    elsif op = MUL then
      printf(1, "mul    r%d, r%d, r%d\n", {a, b, c})
    elsif op = DIV then
      printf(1, "div    r%d, r%d, r%d\n", {a, b, c})
    elsif op = REM then
      printf(1, "rem    r%d, r%d, r%d\n", {a, b, c})
    elsif op = JL then
      printf(1, "jl     r%d, r%d, %d\n", {a, b, c - 2 * and_bits(c, #80)})
    elsif op = JLE then
      printf(1, "jle    r%d, r%d, %d\n", {a, b, c - 2 * and_bits(c, #80)})
    elsif op = JE then
      printf(1, "je     r%d, r%d, %d\n", {a, b, c - 2 * and_bits(c, #80)})
    elsif op = JNE then
      printf(1, "jne    r%d, r%d, %d\n", {a, b, c - 2 * and_bits(c, #80)})
    elsif op = JMP then
      printf(1, "jmp    %d\n", {d - 2 * and_bits(d, #8000)})
    elsif op = EQ then
      printf(1, "eq     r%d, r%d, r%d\n", {a, b, c})
    elsif op = NEQ then
      printf(1, "neq    r%d, r%d, r%d\n", {a, b, c})
    elsif op = REM then
      printf(1, "neq    r%d, r%d, r%d\n", {a, b, c})
    elsif op = QPRINT then
      printf(1, "qprint r%d\n", {a})
    elsif op = END then
      printf(1, "end    \n", {})
    else
      printf(1, "op#%x  r%d, r%d, r%d\n", {op, a, b, c})
    end if
  end for
end procedure

disasm(compile(parse(GLOBAL)) & asm(END,0,0,0))
