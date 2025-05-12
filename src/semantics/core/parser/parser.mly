(* ================================= *)
(*            Definitions            *)
(* ================================= *)

%{
  open EslSyntax
  open EslSyntax.Source
  open Parsing_utils
%}

(* ========== Typed tokens ========== *)

%token <int>    INT
%token <float>  FLOAT
%token <string> STRING
%token <bool>   BOOLEAN
%token <string> SYMBOL
%token <string> ID

(* ========== Language tokens ========== *)

%token NULL
%token FUNCTION
%token PRINT RETURN DELETE EXTERN
%token ASSERT FAIL
%token IF ELSE
%token WHILE
%token SWITCH CASE DEFAULT

(* ========== Operator tokens ========== *)

%token LIST_HEAD LIST_TAIL
%token TYPEOF
%token INT_TO_FLOAT INT_TO_STRING
%token FLOAT_TO_INT FLOAT_TO_STRING
%token STRING_TO_INT STRING_TO_FLOAT
%token OBJECT_TO_LIST OBJECT_FIELDS OBJECT_MEM

(* ========== Symbol tokens ========== *)

%token COMMA SEMICOLON COLON
%token DEFEQ ATSIGN HASH
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token QUESTION EXCLAMATION
%token PLUS MINUS TIMES DIVIDE MODULO POW
%token TILDE AMPERSAND PIPE CARET
%token SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%token LAND LOR
%token EQ NE LT GT LE GE
%token EOF

(* ========== Precedence and associativity ========== *)

%left LAND LOR
%left EQ NE
%left LT GT LE GE
%left AMPERSAND PIPE CARET SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%left MINUS PLUS
%left TIMES DIVIDE MODULO
%left QUESTION
%right COLON
%right POW

%nonassoc unopt_prec



(* ========== Entry point ========== *)

%type <Expr.t> entry_expr_target
%type <Stmt.t> entry_stmt_target
%type <Func.t> entry_func_target
%type <Prog.t> entry_prog_target

%start
entry_expr_target
entry_stmt_target
entry_func_target
entry_prog_target



(* ======================================= *)
(*            Grammar and rules            *)
(* ======================================= *)

%%

let entry_expr_target := ~ = expr_target; EOF; <>
let entry_stmt_target := ~ = stmt_target; EOF; <>
let entry_func_target := ~ = func_target; EOF; <>
let entry_prog_target := ~ = prog_target; EOF; <>

(* ==================== Program  ==================== *)

let prog_target :=
  | ~ = separated_list(SEMICOLON, func_target);
    < Prog.create >

(* ==================== Program Elements ==================== *)

let func_target :=
  | FUNCTION; fn = id_target; pxs = func_param_target; s = block_stmt_target;
    { Func.create fn pxs s @> at $sloc }

(* ==================== Statements ==================== *)

let stmt_target :=
  | ~ = aux_stmt_target;          <>
  | ~ = exec_stmt_target;         <>
  | ~ = update_stmt_target;       <>
  | ~ = block_stmt_target;        <>
  | ~ = selection_stmt_target;    <>
  | ~ = iteration_stmt_target;    <>

let aux_stmt_target :=
  | HASH; s = stmt_target;
    { Stmt.Debug s @> at $sloc }

let exec_stmt_target :=
  | PRINT; e = expr_target;
    { Stmt.Print e @> at $sloc }
  | RETURN; e = expr_target?;
    { Stmt.Return (Parsing_helper.Expr.parse_return_expr e) @> at $sloc }
  | ASSERT; e = expr_target;
    { Stmt.Assert e @> at $sloc }
  | FAIL; e = expr_target;
    { Stmt.Fail e @> at $sloc }

let update_stmt_target :=
  | x = id_target; DEFEQ; e = expr_target;
    { Stmt.Assign (x, e) @> at $sloc }
  | x = id_target; DEFEQ; fn = expr_target; es = call_args_target;
    { Stmt.AssignCall (x, fn, es) @> at $sloc }
  | x = id_target; DEFEQ; EXTERN; fn = id_target; es = call_args_target;
    { Stmt.AssignECall (x, fn, es) @> at $sloc }
  | x = id_target; DEFEQ; LBRACE; RBRACE;
    { Stmt.AssignNewObj x @> at $sloc }
  | x = id_target; DEFEQ; OBJECT_TO_LIST; e = expr_target;
    { Stmt.AssignObjToList (x, e) @> at $sloc }
  | x = id_target; DEFEQ; OBJECT_FIELDS; e = expr_target;
    { Stmt.AssignObjFields (x, e) @> at $sloc }
  | x = id_target; DEFEQ; e1 = expr_target; OBJECT_MEM; e2 = expr_target;
    { Stmt.AssignInObjCheck (x, e1, e2) @> at $sloc }
  | x = id_target; DEFEQ; oe = expr_target; fe = lookup_target;
    { Stmt.FieldLookup (x, oe, fe) @> at $sloc }
  | oe = expr_target; fe = lookup_target; DEFEQ; e = expr_target;
    { Stmt.FieldAssign (oe, fe, e) @> at $sloc }
  | DELETE; oe = expr_target; fe = lookup_target;
    { Stmt.FieldDelete (oe, fe) @> at $sloc }

let block_stmt_target :=
  | LBRACE; ss = separated_list (SEMICOLON, stmt_target); RBRACE;
    { Stmt.Block ss @> at $sloc }

let selection_stmt_target :=
  | IF; e = guard_target; s1 = block_stmt_target;
    { Stmt.If (e, s1, None) @> at $sloc }
  | IF; e = guard_target; s1 = block_stmt_target; ELSE; s2 = block_stmt_target;
    { Stmt.If (e, s1, Some s2) @> at $sloc }
  | SWITCH; e = guard_target; LBRACE; css = switch_case_target*; dflt = switch_dflt_target?; RBRACE;
    { Stmt.Switch (e, (Parsing_helper.Stmt.parse_switch_cases css), dflt) @> at $sloc }

let iteration_stmt_target :=
  | WHILE; e = guard_target; s = block_stmt_target;
    { Stmt.While (e, s) @> at $sloc }

(* ==================== Statement Elements ==================== *)

let guard_target := LPAREN; ~ = expr_target; RPAREN; <>

let switch_case_target := CASE; e = val_expr_target; COLON; s = block_stmt_target; { (e, s) }

let switch_dflt_target := DEFAULT; COLON; ~ = block_stmt_target; <>

(* ==================== Expressions ==================== *)

let expr_target :=
  | LPAREN; ~ = expr_target; RPAREN;    <>
  | ~ = val_expr_target;                <>
  | ~ = var_expr_target;                <>
  | ~ = op_expr_target;                 <>
  | ~ = curry_expr_target;              <>

let val_expr_target :=
  | v = val_target;
    { Expr.Val v @> at $sloc }

let var_expr_target :=
  | x = ID;
    { Expr.Var x @> at $sloc }

let op_expr_target :=
  | op = unopt_target; e = expr_target; %prec unopt_prec
    { Expr.UnOpt (op, e) @> at $sloc }
  | e1 = expr_target; op = binopt_target; e2 = expr_target;
    { Expr.BinOpt (op, e1, e2) @> at $sloc }
  | e1 = expr_target; QUESTION; e2 = expr_target; COLON; e3 = expr_target;
    { Expr.TriOpt (Conditional, e1, e2, e3) @> at $sloc }
  | LBRACK; es = separated_list (COMMA, expr_target); RBRACK;
    { Expr.NOpt (ListExpr, es) @> at $sloc }

let curry_expr_target :=
  | LBRACE; fe = expr_target; RBRACE; ATSIGN; es = call_args_target;
    { Expr.Curry (fe, es) @> at $sloc }

(* ==================== Generic Elements ==================== *)

let func_param_target := LPAREN; ~ = separated_list(COMMA, id_target); RPAREN;  < Parsing_helper.Func.parse_params >

let call_args_target := LPAREN; ~ = separated_list(COMMA, expr_target); RPAREN; <>

let lookup_target := LBRACK; ~ = expr_target; RBRACK; <>

(* ==================== Values ==================== *)

let id_target := x = ID;              { (x @> at $sloc) }

let val_target :=
  | i = INT;                < Value.Int >
  | f = FLOAT;              < Value.Real >
  | s = STRING;             < Value.Str >
  | b = BOOLEAN;            { if b then Value.True else Value.False }
  | s = SYMBOL;             { Value.App (`Op "symbol", [ Value.Str s ]) }
  | NULL;                   { Value.Nothing }

(* ==================== Operators ==================== *)

let unopt_target ==
  | MINUS;                  { Operator.Neg }
  | TILDE;                  { Operator.BitwiseNot }
  | EXCLAMATION;            { Operator.LogicalNot }
  | LIST_HEAD;              { Operator.ListHead }
  | LIST_TAIL;              { Operator.ListTail }
  | TYPEOF;                 { Operator.Typeof }
  | INT_TO_FLOAT;           { Operator.IntToFloat }
  | INT_TO_STRING;          { Operator.IntToString }
  | FLOAT_TO_INT;           { Operator.FloatToInt }
  | FLOAT_TO_STRING;        { Operator.FloatToString }
  | STRING_TO_INT;          { Operator.StringToInt }
  | STRING_TO_FLOAT;        { Operator.StringToFloat }

let binopt_target ==
  | PLUS;                   { Operator.Plus }
  | MINUS;                  { Operator.Minus }
  | TIMES;                  { Operator.Times }
  | DIVIDE;                 { Operator.Div }
  | MODULO;                 { Operator.Modulo }
  | POW;                    { Operator.Pow }
  | AMPERSAND;              { Operator.BitwiseAnd }
  | PIPE;                   { Operator.BitwiseOr }
  | CARET;                  { Operator.BitwiseXor }
  | SHIFT_LEFT;             { Operator.ShiftLeft }
  | SHIFT_RIGHT;            { Operator.ShiftRight }
  | SHIFT_RIGHT_LOGICAL;    { Operator.ShiftRightLogical }
  | LAND;                   { Operator.LogicalAnd }
  | LOR;                    { Operator.LogicalOr }
  | EQ;                     { Operator.Eq }
  | NE;                     { Operator.Ne }
  | LT;                     { Operator.Lt }
  | GT;                     { Operator.Gt }
  | LE;                     { Operator.Le }
  | GE;                     { Operator.Ge }
