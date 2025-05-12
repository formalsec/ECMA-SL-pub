<p align="center" style="font-size: 32px">ECMA-SL to Core ECMA-SL</p>
<p align="center" style="font-size: 20px">Compilation Rules</p>

<br>

This document describes the compilation rules for the ECMA-SL intermediate language. It explains how the ECMA-SL language is compiled into Core ECMA-SL, which is a lower-level for which we provide an interpreter. The document includes the rules for compiling expressions, statements, and functions.

Last modified: January 30, 2024

<br>

# Expressions

The compilation of expressions `e` returns a pair consisting of: (1) a compiled statement list `e_s`, encapsulating a series of statements that must be executed to process the expression; and (2) a compiled expression `e_e`, containing the result of the original expression. For example, the result of compiling the expression `-(foo)`, is the pair `([ __v := -(foo) ], __v)`.
In the following, we describe the expression rules using the format:
```
<requirements>
----------------------------------------
<compiled statement list>
```

Note that compiling an expression usually results in the creation of an internal variable to store the result of the expression.We assume that the compiled expression is the internal variable `res`.

<br>

### Values $\rightarrow$ `v`
The compilation of values returns an expression containing that value.

<br>

### Local Variables $\rightarrow$ `x`
The compilation of variables returns an expression containing that variable.

<br>

### Global Variables $\rightarrow$ `|x|`
- **[R1]:** Generate a fresh variable `res` to store the result of the global variable lookup.
1. Lookup the global variable on the internal ESL global object `___internal_esl_global`, storing the result in `res`.
```
» fresh res
----------------------------------------
compile <|x|> =
  res := ___internal_esl_global["x"]
```

<br>

### Constants $\rightarrow$ `c` 
The compilation of constants returns an expression containing the value associated with tat constant.

<br>

### Unary Operators $\rightarrow$ `op(e)`
- **[R1]:** Compile the argument expression `e`.
- **[R2]:** Generate a fresh variable `res` to store the result of the operator evaluation.
1. Append the compiled argument statement list `e_s`.
2. Apply the operator to the compiled argument expression `e_e`, storing the result in `res`.
    - The `ObjectToList` operator generates a special `.cesl` assignment, rather than a normal operator assignment.
```
» compile e = (e_e, e_s)
» fresh res
----------------------------------------
compile <op(e)> =
  e_s [...];
  res = op(e_e)
```

<br>

### Binary Operators $\rightarrow$ `op(e1, e2)`
- **[R1]:** Compile the argument expressions `e1` and `e2`.
- **[R2]:** Generate a fresh variable `res` to store the result of the operator evaluation.
1. Append the compiled argument statements list `e1_s` and `e2_s`.
2. Apply the operator to the compiled argument expressions `e1_e` and `e2_e`, storing the result in `res`.
    - The short circuit AND `&&&` and OR `|||` operators have a special statement compilation illustrated below.
```
» compile e1 = (e1_e, e1_s)
» compile e2 = (e2_e, e2_s)
» fresh res
----------------------------------------
compile <op(e1, e2)> =
  e1_s [...];
  e2_s [...];
  res = op(e1_e, e2_e)
```

#### Statement compilation of the short circuit (AND) operator $\rightarrow$ `e1 &&& e2`
```
compile <e1 &&& e2> =
  e1_s [...];
  if (e1_e = false) {
    res := false
  } else {
    e2_s [...];
    if (e2_e = false) {
      res := false
    } else {
      res := true
    }
  }
```

#### Statement compilation of the short circuit (OR) operator $\rightarrow$ `e1 ||| e2`
```
compile <e1 ||| e2> =
  e1_s [...];
  if (e1_e = true) {
    res := true
  } else {
    e2_s [...];
    if (e2_e = true) {
      res := true
    } else {
      res := false
    }
  }
```

<br>

### Ternary Operators $\rightarrow$ `op(e1, e2, e3)`
- **[R1]:** Compile the argument expressions `e1`, `e2`, and `e3`.
- **[R2]:** Generate a fresh variable `res` to store the result of the operator evaluation.
1. Append the compiled argument statements list `e1_s`, `e2_s`, and `e3_s`.
2. Apply the operator to the compiled argument expressions `e1_e`, `e2_e`, and `e3_e`, storing the result in `res`.
```
» compile e1 = (e1_s, e1_e)
» compile e2 = (e2_s, e2_e)
» compile e3 = (e3_s, e3_e)
» fresh res
----------------------------------------
compile <op(e1, e2, e3)> =
  e1_s [...];
  e2_s [...];
  e3_s [...];
  res = op(e1_e, e2_e, e3_e)
```

<br>

### NAry Operators $\rightarrow$ `op(e1 ... en)`
- **[R1]:** Compile the argument expressions `e1 ... en`.
- **[R2]:** Generate a fresh variable `res` to store the result of the operator evaluation.
1. Append the compiled argument statements list `e1_s ... en_s`.
2. Apply the operator to the compiled argument expressions `e1_e ... en_e`., storing the result in `res`.
```
» compile ei = (ei_s, ei_e) |i=1^n
» fresh res
----------------------------------------
compile <op(e1 ... en)> =
  ei_s [...]; |i=1^n
  res = op(e1_e ...  en_e)
```

<br>

### Function Call $\rightarrow$ `fe(e1 ... en)`
- **[R1]:** Compile the function expression `fe`.
- **[R2]:** Compile the argument expressions `e1 ... en`.
- **[R3]:** Generate a fresh variable `res` to store the result of the function call.
- **[R4]:** Append the internal ESL global object `___internal_esl_global` to the compiled argument expressions `e1_e ... en_e`.
1. Append the compiled function statement list `fe_s`.
2. Append the compiled argument statements list `e1_s ... en_s`.
3. Call the function with the compiled argument expressions `e1_e ... en_e`, storing the result in `res`.
4. Test the result `res` of the function (which is expected to be a pair):
    - If the first element is `true`, this means that the function thrown an error, and so we return the error message (second element).
    - If the first element is `false`, this means that the function ended normally, and so we assign the function's return  value (second element) to `res`. 
```
» compile fe = (fe_s, fe_e)
» compile ei = (ei_s, ei_e) |i=1^n
» fresh res
----------------------------------------
compile <fe(e1 ... en)> =
  fe_s [...];
  ei_s [...]; |i=1^n
  res := fe_e(___internal_esl_global, e1_e ... en_e);
  if (fst(res)) {
    return res
  } else {
    res := snd(res)
  }
```

#### Statement compilation of function calls with a catch clause $\rightarrow$ `fe(e1 ... en) catch ferr`
When a function call contains a catch clause, the compiled statement list is the same, except that we call the error handler `ferr` when an error is thrown (i.e., when `fst(ret)` evaluates to `true`).
```
compile <fe(e1 ... en) catch ferr> =
  fe_s [...];
  ei_s [...]; |i=1^n
  res := fe_e(___internal_esl_global, e1_e ... en_e);
  if (fst(res)) {
    res := "ferr"(___internal_esl_global, snd(res));
    if (fst(res)) {
      return res
    } else {
      res := snd(res)
    }
  } else {
    res := snd(res)
  }
```

<br>

### External Function Call $\rightarrow$ `extern fn(e1 ... en)`
- **[R1]:** Compile the argument expressions `e1 ... en`.
- **[R2]:** Generate a fresh variable `res` to store the result of the external function call.
1. Append the compiled argument statements list `e1_s ... en_s`.
2. Call the external function with the compiled argument expressions `e1_e ... en_e`, storing the result in `res`.
```
» compile ei = (ei_s, ei_e) |i=1^n
» fresh res
----------------------------------------
compile <fn(e1 ... en)> =
  ei_s [...]; |i=1^n
  res := extern fn(e1_e ... en_e)
```

<br>

### New Object $\rightarrow$ `{ f1: e1 ... fn: en }`
- **[R1]:** Generate a fresh variable `res` to store the result of the new object construction.
- **[R2]:** Compile the field expressions `e1 ... en`.
1. Create an empty object, storing the result in `res`.
2. For each field `f1 ... fn`:
    1. Append the compiled field statement list `ei_s`.
    2. Assign the compiled field expression `ei_e` to field `fi` on the new object.
```
» fresh res
» compile ei = (ei_s, ei_e) |i=1^n
----------------------------------------
compile <{ f1: e1 ... fn: en }> =
  res := {};
  (ei_s [...]; res[fi] := ei_e;) |i=1^n
```

<br>

### Field Lookup $\rightarrow$ `oe[fe]`
- **[R1]:** Compile the object expression `oe`.
- **[R2]:** Compile the field expression `fe`.
- **[R3]:** Generate a fresh variable `res` to store the result of the field lookup.
1. Append the compiled object statement list `oe_s`.
2. Append the compiled field statement list `fe_s`.
3. Lookup the compiled field expression `fe_e` on the compiled object expression `oe_e`, storing the result in `res`.
```
» compile oe = (oe_s, oe_e)
» compile fe = (fe_s, fe_e)
» fresh res
----------------------------------------
compile <oe[fe]> =
  oe_s [...];
  fe_s [...];
  res := oe_e[fe_e]
``` 

<br>

### Curry Expression $\rightarrow$ `fe@(e1 ... en)`
- **[R1]:** Compile the function expression `fe`.
- **[R2]:** Compile the argument expressions `e1 ... en`.
- **[R3]:** Generate a fresh variable `res` to store the result of the curry expression.
1. Append the compiled function statement list `fe_s`.
2. Append the compiled argument statements list `e1_s ... en_s`.
3. Generate a curry expression with the compiled function expression `fe_e` and argument expressions `e1_e ... en_n`, storing the result in `res`.
```
» compile fe = (fe_s, fe_e)
» compile ei = (ei_s, ei_e) |i=1^n
» fresh res
----------------------------------------
compile <fe@(e1 ... en)> =
  fe_s [...];
  e1_s [...]; |i=1^n
  res := fe_e@(e1_e ... en_e);
```

<br>

### Symbolic Expression $\rightarrow$ `se_mk_symbolic(t, e)`
- **[R1]:** Compile the argument expression `e`.
- **[R2]:** Generate a fresh variable `res` to store the result of the symbolic expression.
1. Append the compiled argument statement list `e_s`.
3. Create a symbolic expression with type `t` and the compiled argument expression `e_e`, storing the result in `res`.
```
» compile e = (e_s, e_e)
» fresh res
----------------------------------------
compile <fe@(e1 ... en)> =
  e_s [...];
  res := se_mk_symbolic(t, e_e)
```

<br>
<br>
<br>
<br>

# Statements

The compilation of statements `s` returns a compiled statement list `s_s`, encapsulating a series of statements that must be executed to process the statement. For example, the result of compiling the statement `x := -(foo)`, is the list `[ __v := -(foo); x := __v ]`. In the following, we describe the statement rules using the format:
```
<requirements>
----------------------------------------
<compiled statement list>
```

<br>

### Skip Statement $\rightarrow$ `skip`
The compilation of the skip statement returns another skip statement.

<br>

### Debug Statement $\rightarrow$ `debug(s)`
- **[R1]:** Compile the inner statement `s`.
1. Create a debug statement over the first statement of the compiled statement list `s1_s`.
2. Append the remaining compiled statement list `ss_s`.
```
» compile s = (s1_s :: ss_s)
----------------------------------------
compile <debug(s)> =
  debug(s1_s);
  ss_s [...]
```

<br>

### Block Statement $\rightarrow$ `{ s1; ... sn }`
The compilation of the block statement returns the merged compiled statement list of all statements `s1 ... sn` within the block.

<br>

### Print Statement $\rightarrow$ `print(e)`
- **[R1]:** Compile the argument expression `e`.
1. Append the compiled argument statement list `e_s`.
2. Create a print statement with the compiled argument expression `e_e`.
```
» compile e = (e_s, e_e)
----------------------------------------
compile <print(e)> =
  e_s [...];
  print(e_e)
```

<br>

### Return Statement $\rightarrow$ `return e`
- **[R1]:** Check nothing is being returned (i.e., the returned value is `void`).
    - If so, we set the return expression to the `null` value.
- **[R2]:** Compile the return expression `e`.
1. Append the compiled return statement list `e_s`.
2. Create a return statement with the a pair containing:
    1. The boolean value `false`, representing that the function did not throw an error;
    1. The compiled return expression `e_e`.
```
» e' = (if e == void then null else e)
» compile e' = (e_s, e_e)
----------------------------------------
compile <return e> =
  e_s [...];
  return (false, e_e)
```

<br>

### Expression Statement $\rightarrow$ `e`
- **[R1]:** Compile the return expression `e`.
1. Append the compiled return statement list `e_s`.
```
» compile e = (e_s, e_e)
----------------------------------------
compile <e> =
  e_s [...]
```

<br>

### Local Assignment $\rightarrow$ `x := e`
- **[R1]:** Compile the right value expression `e`.
1. Append the compiled right value statement list `e_s`.
1. Assign the compiled right value expression `e_e` to the identifier `x`.
```
» compile e = (e_s, e_e)
----------------------------------------
compile <x := e> =
  e_s [...];
  x := e_e
```

<br>

### Global Assignment $\rightarrow$ `|x| := e`
- **[R1]:** Compile the right value expression `e`.
1. Append the compiled right value statement list `e_s`.
2. Assign the compiled right value expression `e_e` to the global variable field `x` on the internal ESL global object `___internal_esl_global`.
```
» compile e = (e_s, e_e)
----------------------------------------
compile <|x| := e> =
  e_s [...];
  ___internal_esl_global["x"] := e_e
```

<br>

### Field Assignment $\rightarrow$ `oe[fe] := e`
- **[R1]:** Compile the object expression `oe`.
- **[R2]:** Compile the field expression `fe`.
- **[R3]:** Compile the right value expression `e`.
1. Append the compiled object statement list `oe_s`.
2. Append the compiled field statement list `fe_s`.
3. Append the compiled right value statement list `e_s`.
4. Assign the compiled right value expression `e_e` to the compiled field expression `fe_e` on the compiled object expression `oe_e`.
```
» compile oe = (oe_s, oe_e)
» compile fe = (fe_s, fe_e)
» compile e = (e_s, e_e)
----------------------------------------
compile <oe[fe] := e> =
  oe_s [...];
  fe_s [...];
  e_s [...];
  oe_e[fe_e] := e_e
```

<br>

### Field Delete $\rightarrow$ `delete oe[fe]`
- **[R1]:** Compile the object expression `oe`.
- **[R2]:** Compile the field expression `fe`.
1. Append the compiled object statement list `oe_s`.
2. Append the compiled field statement list `fe_s`.
3. Delete the compiled field expression `fe_e` from the compiled object expression `oe_e`.
```
» compile oe = (oe_s, oe_e)
» compile fe = (fe_s, fe_e)
----------------------------------------
compile <delete oe[fe]> =
  oe_s [...];
  fe_s [...];
  delete oe_e[fe_e]
```

<br>

### If $\rightarrow$ `if (e) {s}`
- **[R1]:** Compile the guard expression `e`.
- **[R2]:** Compile the consequent statement `s`.
1. Append the compiled guard statement list `e_s`.
2. Create an if statement with the compiled guard expression `e_e` and compiled consequent statement list `s_s`
    - The consequent statement needs to be encapsulated in a block statement.
```
» compile e = (e_s, e_e)
» compile s = s_s
----------------------------------------
compile <if (e) {s}> =
  e_s [...];
  if (e_e) { 
    s_s [...] 
  }
```

#### Statement compilation of if statements with an else block $\rightarrow$ `if (e) {s1} else {s2}`
When an if statement contains an else block, the compilation is similar, except that we also compile the statement `s2`, which is then used as the alternate statement of the original if. Note that the alternate statement is compiled before the consequent.
```
» compile s2 = s2_s
» compile e = (e_s, e_e)
» compile s1 = s1_s
----------------------------------------
compile <if (e) {s1} else {s2}> =
  e_s [...];
  if (e_e) { 
    s1_s [...] 
  } else { 
    s2_s [...]
  }
```

#### Statement compilation of if statements with elif blocks $\rightarrow$ `if (e1) {s1} elif (e2) {s2} ... elif (en) {sn} else {s'}`
When an if statement contains elif blocks, the compilation uses the same mechanism described above but transforms the elif blocks into separate if statements, storing them as the alternate block of the preceding case. In other words:
```
if (e1) {
  s1
} elif (e2) {
  s2
} else {
  s'
}
```
is transformed into:
```
if (e1) {
  s1
} else { 
  if (e2) {
    s2
  } else {
    s'
  }
}
```

The compilation for this statement is done recursively, until we are left with a statement without elif clauses and an optional else block.
```
compile <if (e1) {s1} elif (e2) {s2} ... elif (en) {sn} else {s'}> =
  e_s [...];
  if (e_e) { 
    s_s [...] 
  } else { 
    compile <if (e2) {s2} elif ... elif (en) {sn} else s'>
  }
```

<br>

### While Loop $\rightarrow$ `while (e) {s}`
- **[R1]:** Compile the guard expression `e`.
- **[R2]:** Compile the looped statement `s`.
1. Append the compiled guard statement list `e_s`.
2. Create a while loop with the compiled guard expression `e_e` and the looped block.
    - The looped block is a block statement composed of: (1) the compiled looped statement list  `s_s`; and (2) the compiled guard statement list `e_e`.
```
» compile e = (e_s, e_e)
» compile s = s_s
----------------------------------------
compile <while (e) {s}> =
  e_s [...];
  while (e_e) { 
    s_s [...]; 
    e_s [...]
  }
```

<br>

### Foreach Loop $\rightarrow$ `foreach (x : e) {s}`
- **[R1]:** Generate a fresh variable `i` to store the current iteration of the loop.
- **[R2]:** Generate a fresh variable `len` to store the length of the collection `e`.
- **[R3]:** Create a guard statement `guard` defined as the binary operation `len > i`.
- **[R4]:** Compile the collection expression `e`.
- **[R5]:** Compile the guard expression `guard`.
- **[R6]:** Compile the looped statement `s`.
1. Append the compiled collection statement list `e_s`.
2. Initialize variable `i` with 0.
3. Initialize variable `len` with the list length of the compiled collection expression `e_e`.
4. Append the compiled guard statement list `guard_s`.
5. Create a while loop with the compiled guard expression `guard_e` and the looped block.
    - The looped block is a block statement composed of: (1) an assignment of the element in position `i` from the compiled collection expression `e_e` to the identifier `x`; (2) the compiled looped statement list `s_s`; (3) an incrementation of variable `i`; and (4) the compiled guard statement list `guard_s`.
```
» fresh i
» fresh len
» guard = (len > i)
» compile e = (e_s, e_e)
» compile guard = (guard_s, guard_e)
» compile s = s_s
----------------------------------------
compile <foreach (x : e) {s}> =
  e_s [...];
  i = 0;
  len = l_len(e_e);
  guard_s [...];
  while (len > i) {
    x := e_e[i];
    s_s [...];
    i = i + 1;
    guard_s [...];
  }
```

<br>

### Repeat Until Loop $\rightarrow$ `repeat {s} until (e)`
- **[R1]:** Compile the looped statement `s`.
- **[R2]:** Compile the until expression `e`.
1. Append the compiled looped statement list `s_s`.
2. Append the compiled looped statement list `e_s`.
3. Create a while loop with the logical negation (NOT) of the compiled until expression `!(e_e)` and the looped block.
    - The looped block is a block statement composed of: (1) the compiled looped statement list `s_s`; and (2) the compiled until statement list `e_e`.
```
» compile s = s_s
» compile e = (e_s, e_e)
----------------------------------------
compile <repeat {s} until (e)> =
  s_s [...];
  e_s [...];
  while (!(e_e)) {
    s_s [...];
    e_s [...];
  }
```

#### Repeat Loop (without the until block) $\rightarrow$ `repeat {s}`
If there is no until block within the repeat until loop, than we assume the until expression to be the value `false`.
```
compile <repeat {s}> =
  s_s [...];
  while (!false) {
    s_s [...];
  }
```

<br>

### Switch Statement *(without optimization)* $\rightarrow$ `switch(e) { case (e1): {s1} }`
- **[R1]:** Compile the switch-expr expression `e`.
- **[R2]:** Compile the case-test `e1`.
- **[R3]:** Compile the consequent `s1`.
1. Append the compiled switch-expr statement list `e_s`.
2. Append the compiled case-test statement list `e1_s`.
3. Create an if statement with the switch guard and compiled consequent statement list `s1_s`.
    - The switch guard is a comparison between the compiled case-test expression `e1_e` expression and the compiled switch-expr `e_e`.
```
» compile e = (e_s, e_e)
» compile e1 = (e1_s, e1_e)
» compile s1 = s1_s
----------------------------------------
compile <switch(e) { case (e1): {s1} ... case (en): {sn} }> =
  e_s [...];
  e1_s [...];
  if (e1_e = e_e) {
    s1_s [...]
  }
```

#### Statement compilation of switch statements with a default block $\rightarrow$ `switch(e) { case (e1): {s1} sdefault {s2} }`
When an switch statement contains a default block, the compilation is similar, except that we also compile the statement `s2`, which is then used as the alternate statement of the original if. Note that the alternate statement is compiled before the consequent.
```
» compile e = (e_s, e_e)
» compile s2 = s2_s
» compile e1 = (e1_s, e1_e)
» compile s1 = s1_s
----------------------------------------
compile <switch(e) { case (e1): {s1} ... case (en): {sn} }> =
  e_s [...];
  e1_s [...];
  if (e1_e = e_e) {
    s1_s [...]
  } else {
    s2_s [...]
  }
```

#### Statement compilation of switch statements with multiple cases $\rightarrow$ `switch(e) { case (e1): {s1} case (e2): {s2} sdefault {s'} }`
When a switch statement contains multiple cases, the compilation uses the same mechanism described above but transforms each case into a separate if statement, storing them as the alternate block of the preceding case. In other words:
```
switch (e) {
  case: e1 {s1}
  case: e2 {s2}
  sdefault: {s'}
}
```
is transformed into:
```
if (e = e1) {
  s1
} else {
  if (e = e2) {
    s2
  } else {
    s'
  }
}
```

The compilation for this statement is done recursively, until we are left with a single case (and an optional default).
```
compile <switch(e) { case (e1): {s1} case (e2): {s2} sdefault {s'} }> =
  e_s [...];    // this is only compiled for the outer most iteration
  e1_s [...];
  if (e1_e = e_e) {
    s1_s [...]
  } else {
    compile <switch(e) { case (e2): {s2} sdefault {s'} }> 
  }
```

<br>

### Match With Statement *(without optimization)* $\rightarrow$ `match e with | pat -> {s}`
- **[R1]:** Compile the matched expression `e`.
- **[R2]:** Compile the pattern `pat` using the custom pattern compiler.
- **[R3]:** Compile the consequent statement `s`.
1. Append the compiled matched statement list `e_s`.
1. Append the compiled pre-pattern statement list `pre_s`.
3. Create an if statement with the guard and case block 
    - The case block the combination of the pattern statement list `pat_s` and the compiled consequent statement list `s_s`.
```
» compile e = (e_s, e_e)
» compile_pat (e_e, pat) = (pre_s, guard, pat_s)
» compile s = s_s
----------------------------------------
compile <match e with | pat -> {s}> =
  e_s [...];
  pre_s [...];
  if (guard) {
    pat_s [...];
    s_s [...]
  }
```

#### Statement compilation of match with statements with multiple cases $\rightarrow$ `match e with | pat1 -> {s1} ... | patn -> {sn}`
When a match with statement contains multiple cases, the compilation uses the same mechanism described above but transforms each case into a separate if statement, storing them as the alternate block of the preceding case. In other words:
```
match e with
  | pat1 -> {s1}
  | pat2 -> {s2}
```
is transformed into:
```
if (guard1) {
  s1
} else {
  if (guard2) {
    s2
  }
}
```

The compilation for this statement is done recursively, until there are no cases.
```
compile <match e with | pat1 -> {s1} ... | patn -> {sn}> =
  e_s [...];    // this is only compiled for the outer most iteration
  pre_s [...];
  if (guard) {
    pat_s [...];
    s_s [...]
  } else {
    compile <match e with | pat2 -> {s2} ... | patn -> {sn}>
  }
```

#### Custom pattern compiler $\rightarrow$ `(e_e, pat)`
The compilation of patterns `pat`, considering the matched expression `e_e`, returns a triple consisting of: (1) a compiled statement list `pre_s` containing statements that precede the if statement generated to test the expression against the pattern; (2) a compiled expression `guard_e` containing the test expression for the pattern; and (3) the compiled statement list `pat_s` containing the statement that need to be executed after successfully matching the pattern.

In the following, we describe the pattern rules using the format:
```
<requirements>
----------------------------------------
<pre_s> ; <guard_e> ; <pat_s>
```

- #### Custom pattern compiler (default patterns) $\rightarrow$ `pat = default`
  ```
  [] ; true ; []
  ```

- #### Custom pattern compiler (object patterns) $\rightarrow$ `pat = { pbn1: pbv1 ... pbnn: pbvn }`
  ```
  » compile_patv (e_e, pbni, pbvi) = (prei_s, guardi_xs, pati_s) |i=1^n
  » guardi_xs = guardi_x1 ... guardi_xm | i=1^n
  ----------------------------------------
  prei_s [...] |i=1^n ;
  (true && guardi_x1 && ... && guardi_xm |i=1^n);
  pati_s [...] |i=1^n
  ```

#### Custom pattern value compiler $\rightarrow$ `(e_e, pbn, pbv)`
The compilation of patterns values `(pbn, pbv)`, considering the matched expression `e_e`, returns a triple consisting of: (1) a compiled statement list `pre_s` containing statements that precede the if statement generated to test the expression against the pattern; (2) list of variables `guard1 ... guardn` that need to be true for a successful pattern match; and (3) the compiled statement list `pat_s` containing the statement that need to be executed after successfully matching the pattern.

In the following, we describe the pattern rules using the format:
```
<requirements>
----------------------------------------
<pre_s> ; <guard1 ... guardn> ; <pat_s>
```

- #### Custom pattern value compiler (variables) $\rightarrow$ `pbv = x`
  ```
  » fresh inobj
  ----------------------------------------
  [ inobj := (pbn in_obj e_e) ];
  [ inobj ];
  [ x := e_e[pbn] ]
  ```

- #### Custom pattern value compiler (values) $\rightarrow$ `pbv = v`
  ```
  » fresh inobj
  » fresh fval
  » fresh feq
  ----------------------------------------
  [ inobj := (pbn in_obj e_e) ; fval := e_e[pbn] ; feq := (fval = v) ];
  [ inobj; feq ];
  []
  ```

- #### Custom pattern value compiler (none) $\rightarrow$ `pbv = None`
  ```
  » fresh inobj
  ----------------------------------------
  [ inobj := (pbn in_obj e_e) ; inobj := !(inobj) ];
  [ inobj ];
  []
  ```

<br>

### Lambda Call $\rightarrow$ `x := lambda<id> (pxs) [ctxvars] {s}`
1. Assign a curry expression to the identifier `x`.
    - The curry has the function expression `id` and parameters `ctxvars`.
```
compile <x := lambda<id> (pxs) [ctxvars] {s}> =
  x := {"id"}@(ctxvars)
```

<br>

### Throw Statement $\rightarrow$ `throw e`
- **[R1]:** Compile the throw expression `e`.
1. Append the compiled throw statement list `e_s`.
2. Create a return statement with the a pair containing:
    1. The boolean value `true`, representing that the function threw an error;
    1. The compiled throw expression `e_e`.
```
» compile e = (e_s, e_e)
----------------------------------------
compile <throw e> =
  e_s [...];
  return (true, e_e)
```

<br>

### Fail Statement $\rightarrow$ `fail e`
- **[R1]:** Compile the fail expression `e`.
1. Append the compiled fail statement list `e_s`.
2. Create a fail statement with the compiled fail expression `e_e`.
```
» compile e = (e_s, e_e)
----------------------------------------
compile <fail e> =
  e_s [...];
  fail e_e
```

<br>

### Assert Statement $\rightarrow$ `assert e`
- **[R1]:** Compile the assert expression `e`.
1. Append the compiled assert statement list `e_s`.
2. Create an assert statement with the compiled assert expression `e_e`.
```
» compile e = (e_s, e_e)
----------------------------------------
compile <assert e> =
  e_s [...];
  assert e_e
```

<br>

### Wrapper $\rightarrow$ `gen_wrapper s`
The compilation of the wrapper statement returns the compiled inner statement.

<br>
<br>
<br>
<br>

# Functions

### Normal Function $\rightarrow$ `f(px1... pxn) {s}`
- **[R1]:** Compile the function body `s`.
1. Create a function named `f`, with compiled parameters and the compiled function body `s_s`.
    - The compiled parameters are the combination of the function formal parameters `px1 ... pxn` with the internal ESL global object `___internal_esl_global`.
```
compile s = s_s
----------------------------------------
compile <f(px1... pxn) {s}> =
  function f(___internal_esl_global, x1 ... xn) {
    s_s [...]
  }
```

<br>

### Main Function $\rightarrow$ `main() {s}`
- **[R1]:** Compile the function body `s`.
1. Create a function named `f`, with no parameters and the compiled function body `s_s` preceded by the initialization of the internal ESL global object `___internal_esl_global`.
```
compile s = s_s
----------------------------------------
compile <main() {s}> =
  function main() {
    ___internal_esl_global := {};
    s_s [...]
  }
```
