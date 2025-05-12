(* ================================= *)
(*            Definitions            *)
(* ================================= *)

%{
  open EslSyntax
  open EslSyntax.Source
  open Parsing_utils

  let fresh_lambda_id_gen = EslBase.Base.make_name_generator "__lambda__"
%}

(* ========== Typed tokens ========== *)

%token <int>    INT
%token <float>  FLOAT
%token <string> STRING
%token <bool>   BOOLEAN
%token <string> SYMBOL
%token <string> ID
%token <string> GID

(* ========== Language tokens ========== *)

%token NULL NONE
%token LET
%token IMPORT TYPEDEF MACRO FUNCTION
%token PRINT RETURN DELETE EXTERN LAMBDA
%token ASSERT FAIL THROW CATCH
%token IF ELSE
%token WHILE FOREACH REPEAT UNTIL
%token SWITCH CASE DEFAULT
%token MATCH WITH

(* ========== Operator tokens ========== *)

%token LIST_HEAD LIST_TAIL
%token TYPEOF
%token INT_TO_FLOAT INT_TO_STRING
%token FLOAT_TO_INT FLOAT_TO_STRING
%token STRING_TO_INT STRING_TO_FLOAT
%token OBJECT_TO_LIST OBJECT_FIELDS OBJECT_MEM

(* ========== Symbol tokens ========== *)

%token COMMA SEMICOLON COLON PERIOD
%token DEFEQ0 DEFEQ ATSIGN HASH ARROW
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token QUESTION EXCLAMATION
%token PLUS MINUS TIMES DIVIDE MODULO POW
%token TILDE AMPERSAND PIPE CARET
%token SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%token LAND LOR SCLAND SCLOR
%token EQ NE LT GT LE GE
%token EOF

(* ========== Type system tokens ========== *)

%token TYPE_ANY, TYPE_UNKNOWN, TYPE_NEVER
%token TYPE_UNDEFINED, TYPE_VOID
%token TYPE_INT, TYPE_FLOAT, TYPE_STRING, TYPE_BOOLEAN TYPE_SYMBOL
%token TYPE_SIGMA

(* ========== Precedence and Associativity ========== *)

%right nary_type_prec (* FIXME *)
%nonassoc simple_if_prec simple_match_prec simple_repeat_prec
%nonassoc ELSE UNTIL

%left LAND LOR SCLAND SCLOR
%left EQ NE
%left LT GT LE GE
%left AMPERSAND PIPE CARET SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%left OBJECT_MEM
%left MINUS PLUS
%left TIMES DIVIDE MODULO
%left QUESTION
%right COLON
%right POW

%nonassoc unopt_prec
%nonassoc PERIOD LBRACK



(* ========== Entry Point ========== *)

%type <EExpr.t> entry_expr_target
%type <EStmt.t> entry_stmt_target
%type <EFunc.t> entry_func_target
%type <EType.t> entry_type_target
%type <EProg.t> entry_prog_target

%start
entry_expr_target
entry_stmt_target
entry_func_target
entry_type_target
entry_prog_target



(* ======================================= *)
(*            Grammar and rules            *)
(* ======================================= *)

%%

let entry_expr_target := ~ = expr_target; EOF; <>
let entry_stmt_target := ~ = stmt_target; EOF; <>
let entry_func_target := ~ = func_target; EOF; <>
let entry_type_target := ~ = type_target; EOF; <>
let entry_prog_target := ~ = prog_target; EOF; <>

(* ==================== Program  ==================== *)

let prog_target :=
  | imports = import_target*; prog_els = prog_element_target*;
    { EParsing_helper.Prog.parse_prog imports prog_els }

let prog_element_target :=
  | ~ = tdef_target;    < EParsing_helper.Prog.parse_tdef >
  | ~ = macro_target;   < EParsing_helper.Prog.parse_macro >
  | ~ = func_target;    < EParsing_helper.Prog.parse_func >

(* ==================== Program Elements ==================== *)

let import_target :=
  | IMPORT; import = str_id_target; SEMICOLON;  { EImport.User import @> at $sloc }
  | IMPORT; import = id_target; SEMICOLON;      { EImport.Standard import @> at $sloc }

let tdef_target :=
  | TYPEDEF; tn = id_target; DEFEQ; tv = type_target; SEMICOLON;
    { EType.TDef.create tn tv }

let macro_target :=
  | MACRO; mn = id_target; pxs = func_params_target; s = block_stmt_target;
   { EMacro.create mn pxs s @> at $sloc }

let func_target :=
  | FUNCTION; fn = id_target; pxs = func_tparams_target; tret = typing_target?; s = block_stmt_target;
    { EFunc.create fn pxs tret s @> at $sloc }

(* ==================== Statements ==================== *)

let stmt_target :=
  | ~ = aux_stmt_target;                <>
  | ~ = expr_stmt_target; SEMICOLON;    <>
  | ~ = exec_stmt_target; SEMICOLON;    <>
  | ~ = update_stmt_target; SEMICOLON;  <>
  | ~ = block_stmt_target;              <>
  | ~ = selection_stmt_target;          <>
  | ~ = iteration_stmt_target;          <>

let aux_stmt_target :=
  | HASH; s = stmt_target;
    { EStmt.Debug s @> at $sloc }

let expr_stmt_target :=
  | e = no_blocklike_expr_target;
    { EStmt.ExprStmt e @> at $sloc }

let exec_stmt_target :=
  | PRINT; e = expr_target;
    { EStmt.Print e @> at $sloc }
  | RETURN; e = expr_target?;
    { EStmt.Return (EParsing_helper.Expr.parse_return_expr e) @> at $sloc }
  | ASSERT; e = expr_target;
    { EStmt.Assert e @> at $sloc }
  | FAIL; e = expr_target;
    { EStmt.Fail e @> at $sloc }
  | THROW; e = expr_target;
    { EStmt.Throw e @> at $sloc }
  | ATSIGN; mn = id_target; es = call_args_target;
    { EStmt.MacroApply (mn, es) @> at $sloc }

let update_stmt_target :=
  | LET; id = id_target; DEFEQ0; expr = expr_target;
    (* let-bindings should create immutable assignments *)
    { EStmt.Assign (id, None, expr) @> at $sloc }
  | x = id_target; DEFEQ; e = expr_target;
    { EStmt.Assign (x, None, e) @> at $sloc }
  | x = gid_target; DEFEQ; e = expr_target;
    { EStmt.GAssign (x, e) @> at $sloc }
  | oe = no_blocklike_expr_target; fe = lookup_target; DEFEQ; e = expr_target;
    { EStmt.FieldAssign (oe, fe, e) @> at $sloc }
  | DELETE; oe = expr_target; fe = lookup_target;
    { EStmt.FieldDelete (oe, fe) @> at $sloc }
  | x = id_target; DEFEQ; LAMBDA; pxs = func_params_target;
    ctxvars = ctx_vars_target; s = block_stmt_target;
    { EStmt.Lambda (x, fresh_lambda_id_gen (), pxs, ctxvars, s) @> at $sloc }

let block_stmt_target :=
  | SEMICOLON;
    { EStmt.Skip @> at $sloc }
  | LBRACE; ss = stmt_target*; RBRACE;
    { EStmt.Block ss @> at $sloc }

let selection_stmt_target :=
  | IF; e = guard_target; s1 = stmt_target; %prec simple_if_prec
    { EStmt.If (e, s1, None) @> at $sloc }
  | IF; e = guard_target; s1 = stmt_target; ELSE; s2 = stmt_target;
    { EStmt.If (e, s1, Some s2) @> at $sloc }
  | SWITCH; e = guard_target; LBRACE; css = switch_case_target*; dflt = switch_default_target?; RBRACE;
    { EStmt.Switch (e, css, dflt) @> at $sloc }
  | MATCH; e = expr_target; dsc = match_discrm_target?; WITH; css = match_cases_target;
    { EStmt.MatchWith (e, dsc, css) @> at $sloc }

let iteration_stmt_target :=
  | WHILE; e = guard_target; s = stmt_target;
    { EStmt.While (e, s) @> at $sloc }
  | FOREACH; LPAREN; x = id_target; COLON; e = expr_target; RPAREN; s = stmt_target;
    { EStmt.ForEach (x, e, s) @> at $sloc }
  | REPEAT; s = stmt_target; %prec simple_repeat_prec
    { EStmt.RepeatUntil (s, None) @> at $sloc }
  | REPEAT; s = stmt_target; UNTIL; e = guard_target; SEMICOLON;
    { EStmt.RepeatUntil (s, Some e) @> at $sloc }

(* ==================== Statement Elements ==================== *)

let guard_target := LPAREN; ~ = expr_target; RPAREN; <>

let switch_case_target := CASE; ~ = expr_target; COLON; ~ = stmt_target; <>

let switch_default_target := DEFAULT; COLON; ~ = stmt_target; <>

let match_discrm_target := COLON; ~ = id_target; <>

let match_cases_target :=
  | cs = match_case_target; %prec simple_match_prec { [ cs ] }
  | cs = match_case_target; css = match_cases_target; { cs :: css }

let match_case_target := PIPE; ~ = pattern_target; ARROW; ~ = stmt_target; <>

(* ==================== Pattern Elements ==================== *)

let pattern_target :=
  | LBRACE; pbs = separated_nonempty_list(COMMA, pattern_binding_target); RBRACE;
    { EPat.ObjPat (pbs) @> at $sloc }
  | DEFAULT;
    { EPat.DefaultPat @> at $sloc }

let pattern_binding_target :=
  | ~ = id_target; COLON; ~ = pattern_value_target;       <>
  | ~ = str_id_target; COLON; ~ = pattern_value_target;   <>

let pattern_value_target :=
  | x = id_target;        { EPat.PatVal.Var x.it @> at $sloc }
  | v = val_target;       { EPat.PatVal.Val v @> at $sloc }
  | LBRACK; RBRACK;       { EPat.PatVal.Val (Value.List []) @> at $sloc }
  | NONE;                 { EPat.PatVal.None @> at $sloc }

(* ==================== Expressions ==================== *)

let expr_target ==
  | ~ = no_blocklike_expr_target;       <>
  | ~ = obj_expr_target;                <>

let no_blocklike_expr_target :=
  | LPAREN; ~ = expr_target; RPAREN;    <>
  | ~ = val_expr_target;                <>
  | ~ = var_expr_target;                <>
  | ~ = op_expr_target;                 <>
  | ~ = call_expr_target;               <>

let val_expr_target :=
  | v = val_target;
    { EExpr.Val v @> at $sloc }

let var_expr_target :=
  | x = id_target;
    { EExpr.Var x.it @> at $sloc }
  | x = gid_target;
    { EExpr.GVar x.it @> at $sloc }
  | oe = no_blocklike_expr_target; fe = lookup_target;
    { EExpr.Lookup (oe, fe) @> at $sloc }

let op_expr_target :=
  | op = unopt_target; e = expr_target; %prec unopt_prec
    { EExpr.UnOpt (op, e) @> at $sloc }
  | e1 = no_blocklike_expr_target; op = binopt_target; e2 = expr_target;
    { EExpr.BinOpt (op, e1, e2) @> at $sloc }
  | e1 = no_blocklike_expr_target; QUESTION; e2 = expr_target; COLON; e3 = expr_target;
    { EExpr.TriOpt (Conditional, e1, e2, e3) @> at $sloc }
  | LBRACK; es = separated_list (COMMA, expr_target); RBRACK;
    { EExpr.NOpt (ListExpr, es) @> at $sloc }

let call_expr_target :=
  | fn = id_target; es = call_args_target; ferr = catch_target?;
    { EExpr.Call (EExpr.Val (Str fn.it) @> fn.at, es, ferr) @> at $sloc }
  | LBRACE; fe = no_blocklike_expr_target; RBRACE; es = call_args_target; ferr = catch_target?;
    { EExpr.Call (fe, es, ferr) @> at $sloc }
  | EXTERN; fn = id_target; es = call_args_target;
    { EExpr.ECall (fn, es) @> at $sloc }
  | LBRACE; fe = no_blocklike_expr_target; RBRACE; ATSIGN; es = call_args_target;
    { EExpr.Curry (fe, es) @> at $sloc }

let obj_expr_target :=
  | LBRACE; flds = separated_list(COMMA, field_init_target); RBRACE;
    { EExpr.NewObj (EParsing_helper.Expr.parse_object_fields flds) @> at $sloc }

(* ==================== Generic Elements ==================== *)

let func_tparams_target := LPAREN; ~ = separated_list(COMMA, typed_id_target); RPAREN; < EParsing_helper.Func.parse_params >

let func_params_target := LPAREN; ~ = separated_list(COMMA, id_target); RPAREN; <>

let ctx_vars_target := LBRACK; ~ = separated_list(COMMA, id_target); RBRACK; <>

let call_args_target := LPAREN; ~ = separated_list(COMMA, expr_target); RPAREN; <>

let field_init_target :=
  | ~ = id_target; COLON; ~ = expr_target;      <>
  | ~ = str_id_target; COLON; ~ = expr_target;  <>

let lookup_target :=
  | PERIOD; fn = id_target;                     { EExpr.Val (Value.Str fn.it) @> at $sloc }
  | LBRACK; ~ = expr_target; RBRACK;            <>

let catch_target := CATCH; ~ = id_target;       <>

(* ==================== Values ==================== *)

let id_target := x = ID;          { (x @> at $sloc) }

let gid_target := x = GID;        { (x @> at $sloc) }

let str_id_target := s = STRING;  { (s @> at $sloc) }

let times_id_target := TIMES;     { ("*" @> at $sloc) }

let val_target :=
  | i = INT;             < Value.Int >
  | f = FLOAT;           < Value.Real >
  | s = STRING;          < Value.Str >
  | b = BOOLEAN;         { if b then Value.True else Value.False }
  | s = SYMBOL;          { Value.App (`Op "symbol", [Value.Str s])}
  | NULL;                { Value.Nothing }

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
  | OBJECT_TO_LIST;         { Operator.ObjectToList }
  | OBJECT_FIELDS;          { Operator.ObjectFields }

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
  | SCLAND;                 { Operator.SCLogicalAnd }
  | SCLOR;                  { Operator.SCLogicalOr }
  | EQ;                     { Operator.Eq }
  | NE;                     { Operator.Ne }
  | LT;                     { Operator.Lt }
  | GT;                     { Operator.Gt }
  | LE;                     { Operator.Le }
  | GE;                     { Operator.Ge }
  | OBJECT_MEM;             { Operator.ObjectMem }










(* ==================== FIXME: Type system ==================== *)

let typed_id_target := ~ = id_target; ~ = typing_target?; <>

let typing_target := COLON; t = type_target; <>

let type_target :=
  | LPAREN; t = type_target; RPAREN;      { t }
  | TYPE_ANY;                             { EType.AnyType @> at $sloc }
  | TYPE_UNKNOWN;                         { EType.UnknownType @> at $sloc }
  | TYPE_NEVER;                           { EType.NeverType @> at $sloc }
  | TYPE_UNDEFINED;                       { EType.UndefinedType @> at $sloc }
  | NULL;                                 { EType.NullType @> at $sloc }
  | TYPE_VOID;                            { EType.VoidType @> at $sloc }
  | TYPE_INT;                             { EType.IntType @> at $sloc }
  | TYPE_FLOAT;                           { EType.FloatType @> at $sloc }
  | TYPE_STRING;                          { EType.StringType @> at $sloc }
  | TYPE_BOOLEAN;                         { EType.BooleanType @> at $sloc }
  | TYPE_SYMBOL;                          { EType.SymbolType @> at $sloc }
  | lt = literal_type_target;             { EType.LiteralType (LitStrong, lt) @> at $sloc }
  | ot = object_type_target;              { EType.ObjectType ot @> at $sloc }
  | t = type_target; LBRACK; RBRACK;      { EType.ListType t @> at $sloc }
  | ts = rev(tuple_type_target);          { EType.TupleType(ts) @> at $sloc }     %prec nary_type_prec
  | ts = rev(union_type_target);          { EType.UnionType(ts) @> at $sloc }     %prec nary_type_prec
  | t = sigma_type_target;                { t @> at $sloc }
  | tvar = id_target;                     { EType.UserDefinedType tvar.it @> at $sloc }

let literal_type_target :=
  | i = INT;                              { EType.IntegerLit i }
  | f = FLOAT;                            { EType.FloatLit f }
  | s = STRING;                           { EType.StringLit s }
  | b = BOOLEAN;                          { EType.BooleanLit b }
  | s = SYMBOL;                           { EType.SymbolLit s }

let object_type_target :=
  | LBRACE; props = separated_list (COMMA, object_type_prop_target); RBRACE;
    { EParsing_helper.Type.parse_tobject props }

let object_type_prop_target :=
  | fn = id_target; COLON; t = type_target;             { (fn, t, EType.FldReq) }
  | fn = id_target; QUESTION; COLON; t = type_target;   { (fn, t, EType.FldOpt) }
  | fn = times_id_target; COLON; t = type_target;       { (fn, t, EType.FldReq) }

let tuple_type_target :=
  | t1 = type_target; TIMES; t2 = type_target;          { [t2 ; t1] }
  | ts = tuple_type_target; TIMES; t = type_target;     { t :: ts }

let union_type_target :=
  | t1 = type_target; PIPE; t2 = type_target;           { [t2 ; t1] }
  | ts = union_type_target; PIPE; t = type_target;      { t :: ts }

let sigma_type_target :=
  | TYPE_SIGMA; LBRACK; dsc = id_target; RBRACK; PIPE?; t = type_target;          %prec nary_type_prec
    { EType.SigmaType (dsc, (EParsing_helper.Type.parse_tsigma dsc t)) }
