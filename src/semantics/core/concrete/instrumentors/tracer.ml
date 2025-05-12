(* Copyright (C) 2022-2025 formalsec programmers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *)

open Prelude
open EslBase
open EslSyntax

module Config = struct
  let trace_loc : bool ref = ref false

  let trace_depth : int option ref = ref None

  let max_obj_depth : int = 4

  let width : int = Terminal.width Unix.stderr
end

module Truncate = struct
  let prepare (str_el : 'a -> string) (el : 'a) : string * int =
    str_el el |> fun s -> (s, String.length s)

  let limit_indent (lvl : int) : int = Config.width - (lvl * 2)

  let limit_el (lim : int) (factor : int) (weight : int) (rest : int) : int =
    max (lim * weight / factor) (lim - rest)

  let pp (lim : int) (pp_el : 'a Fmt.t) (ppf : Format.formatter) (el : 'a) :
    unit =
    let extra = Font.str_text_err [ Font.Faint ] "..." in
    let text = Fmt.str "%a" pp_el el in
    let (text', trunc) = String.truncate (lim - 3) text in
    Fmt.string ppf (if trunc then String.cat text' extra else text')
end

type obj = Value.t Object.t

type heap = Value.t Heap.t

type heapval = heap * Value.t

let indent_pp (ppf : Format.formatter) (lvl : int) : unit =
  let indent = Array.make lvl "| " |> Array.to_list |> String.concat "" in
  Font.pp_text_err [ Faint ] ppf indent

let at_pp (limit : int) (ppf : Format.formatter) (at : Source.at) : unit =
  let open Source in
  let pp_at' ppf at = Fmt.pf ppf "(%s:%d)" at.file at.lpos.line in
  Font.pp_err [ Italic; Faint ] (Truncate.pp limit pp_at') ppf at

let cond_at_pp (lvl : int) (ppf : Format.formatter) (at : Source.at) : unit =
  let limit = Truncate.limit_indent lvl in
  let pp ppf () = Fmt.pf ppf "\n%a%a" indent_pp lvl (at_pp limit) at in
  if !Config.trace_loc then pp ppf ()

let rec heapval_pp ?(depth : int = 0) (heap : heap) (ppf : Format.formatter)
  (v : Value.t) : unit =
  match v with
  | App (`Op "loc", [ Int l ]) when depth < Config.max_obj_depth -> (
    match Heap.get heap l with
    | Some obj -> Object.pp (heapval_pp ~depth:(depth + 1) heap) ppf obj
    | None -> Fmt.string ppf "{ ??? }" )
  | _ -> Value.pp ppf v

let val_pp (limit : int) (ppf : Format.formatter) (v_str : string) : unit =
  (Font.pp_err [ Cyan ] (Truncate.pp limit Fmt.string)) ppf v_str

let heapval (heap : heap) (v : Value.t) : string =
  Fmt.str "%a" (heapval_pp heap) v

module CallFmt = struct
  let pp_func_restore (ppf : Format.formatter) (f : Func.t) : unit =
    let limit = Truncate.limit_indent 0 - 8 in
    Fmt.pf ppf "%a started%a"
      (Font.pp_err [ Cyan ] (Truncate.pp limit Fmt.string))
      (Func.name' f) (cond_at_pp 1) f.at

  let pp_func_call code (ppf : Format.formatter) ((lvl, s) : int * Stmt.t) :
    unit =
    let limit = Truncate.limit_indent lvl - 7 in
    let pp_stmt = Font.pp_err [ Cyan ] (Truncate.pp limit Code_utils.pp) in
    Fmt.pf ppf "%a%a called%a" indent_pp lvl pp_stmt (code, s.at)
      (cond_at_pp (lvl + 1))
      s.at

  let retval_format (v : Value.t) : string * Value.t =
    match v with
    | Value.List [ Value.False; v' ] -> ("returned ", v')
    | Value.List [ Value.True; err ] -> ("throwed ", err)
    | _ -> ("returned ", v)

  let pp_func_return (heap : heap) (ppf : Format.formatter)
    ((lvl, f, s, v) : int * Func.t * Stmt.t * Value.t) : unit =
    let (retval_header, v') = retval_format v in
    let (fn_str, fn_len) = Truncate.prepare Func.name' f in
    let (v_str, v_len) = Truncate.prepare (heapval heap) v' in
    let limit = Truncate.limit_indent lvl - String.length retval_header - 1 in
    let limit_f = Truncate.limit_el limit 4 3 v_len in
    let limit_v = Truncate.limit_el limit 4 1 fn_len in
    Fmt.pf ppf "%a%a %s%a%a" indent_pp lvl
      (Font.pp_err [ Cyan ] (Truncate.pp limit_f Fmt.string))
      fn_str retval_header (val_pp limit_v) v_str (cond_at_pp lvl) s.at
end

module type CODE_FMT = sig
  val log_expr : Expr.t -> bool

  val log_stmt : Stmt.t -> bool

  val expr_str : Code_utils.t -> Expr.t -> string

  val stmt_pp : Code_utils.t -> Stmt.t Fmt.t
end

module DefaultFmt (CodeFmt : CODE_FMT) = struct
  module CodeFmt = CodeFmt

  let pp_expr code (heap : heap) (ppf : Format.formatter)
    ((lvl, e, v) : int * Expr.t * Value.t) : unit =
    let lvl' = lvl + 1 in
    let (e_str, e_len) = Truncate.prepare (CodeFmt.expr_str code) e in
    let (v_str, v_len) = Truncate.prepare (heapval heap) v in
    let limit = Truncate.limit_indent lvl' - 11 in
    let limit_e = Truncate.limit_el limit 2 1 v_len in
    let limit_v = Truncate.limit_el limit 2 1 e_len in
    let pp_eval = Font.pp_text_err [ Italic ] in
    let pp_expr = Truncate.pp limit_e Fmt.string in
    Fmt.pf ppf "%a- %a %a -> %a" indent_pp lvl' pp_eval "eval" pp_expr e_str
      (val_pp limit_v) v_str

  let pp_stmt code (ppf : Format.formatter) ((lvl, s) : int * Stmt.t) : unit =
    let lvl' = lvl + 1 in
    let limit = Truncate.limit_indent lvl' in
    let pp_stmt =
      Font.pp_err [ Cyan ] (Truncate.pp limit (CodeFmt.stmt_pp code))
    in
    Fmt.pf ppf "%a%a%a" indent_pp lvl' pp_stmt s (cond_at_pp lvl') s.at

  let pp_func (header : string) (ppf : Format.formatter)
    ((lvl, f) : int * Func.t) : unit =
    let limit = Truncate.limit_indent lvl - String.length header - 1 in
    let pp_fname = Font.pp_err [ Cyan ] (Truncate.pp limit Fmt.string) in
    Fmt.pf ppf "%a%s %a" indent_pp lvl header pp_fname (Func.name' f)
end

let log_level (lvl : int) : bool =
  let log_lvl' max_lvl = lvl < max_lvl in
  Option.fold ~none:true ~some:log_lvl' !Config.trace_depth

module EslCodeFmt : CODE_FMT = struct
  let log_expr (e : Expr.t) : bool =
    e.at.real && match e.it with Val _ -> false | _ -> true

  let log_stmt (s : Stmt.t) : bool =
    s.at.real && match s.it with Skip | Merge | Block _ -> false | _ -> true

  let expr_str code (e : Expr.t) : string = Code_utils.str (code, e.at)

  let stmt_pp code (ppf : Format.formatter) (s : Stmt.t) : unit =
    Code_utils.pp ppf (code, s.at)
end

module CeslCodeFmt : CODE_FMT = struct
  let log_expr (e : Expr.t) : bool =
    match e.it with Val _ -> false | _ -> true

  let log_stmt (s : Stmt.t) : bool =
    match s.it with Skip | Merge | Block _ -> false | _ -> true

  let expr_str _ (e : Expr.t) : string = Expr.str e

  let stmt_pp _ (ppf : Format.formatter) (s : Stmt.t) : unit = Stmt.pp ppf s
end

module type M = sig
  val trace_expr : Code_utils.t -> int -> Expr.t -> heapval -> unit

  val trace_stmt : Code_utils.t -> int -> Stmt.t -> unit

  val trace_restore : int -> Func.t -> unit

  val trace_call : Code_utils.t -> int -> Func.t -> Stmt.t -> unit

  val trace_return : int -> Func.t -> Stmt.t -> heapval -> unit
end

module Disable : M = struct
  let trace_expr _ (_ : int) (_ : Expr.t) (_ : heapval) : unit = ()

  let trace_stmt _ (_ : int) (_ : Stmt.t) : unit = ()

  let trace_restore (_ : int) (_ : Func.t) : unit = ()

  let trace_call _ (_ : int) (_ : Func.t) (_ : Stmt.t) : unit = ()

  let trace_return (_ : int) (_ : Func.t) (_ : Stmt.t) (_ : heapval) : unit = ()
end

module Call : M = struct
  open CallFmt

  let trace_expr _ (_ : int) (_ : Expr.t) (_ : heapval) : unit = ()

  let trace_stmt _ (_ : int) (_ : Stmt.t) : unit = ()

  let trace_restore (lvl : int) (f : Func.t) : unit =
    if lvl = ~-1 then Log.stderr "%a@." pp_func_restore f

  let trace_call code (lvl : int) (_ : Func.t) (s : Stmt.t) : unit =
    if log_level lvl then Log.stderr "%a@." (pp_func_call code) (lvl, s)

  let trace_return (lvl : int) (f : Func.t) (s : Stmt.t) ((heap, v) : heapval) :
    unit =
    if log_level lvl then Log.stderr "%a@." (pp_func_return heap) (lvl, f, s, v)
end

module Step : M = struct
  open DefaultFmt (EslCodeFmt)

  open CodeFmt

  let trace_expr _ (_ : int) (_ : Expr.t) (_ : heapval) : unit = ()

  let trace_stmt code (lvl : int) (s : Stmt.t) : unit =
    if log_level lvl && log_stmt s then Log.stderr "%a@." (pp_stmt code) (lvl, s)

  let trace_restore (lvl : int) (f : Func.t) : unit =
    match lvl with
    | -1 -> Log.stderr "%a@." (pp_func "starting on function") (0, f)
    | _ ->
      if log_level lvl then
        Log.stderr "%a@." (pp_func "returning to function") (lvl, f)

  let trace_call _ (lvl : int) (f : Func.t) (_ : Stmt.t) : unit =
    if log_level lvl then
      Log.stderr "%a@." (pp_func "entering function") (lvl, f)

  let trace_return (lvl : int) (f : Func.t) (_ : Stmt.t) (_ : heapval) : unit =
    if log_level lvl then Log.stderr "%a@." (pp_func "exiting function") (lvl, f)
end

module Full : M = struct
  open DefaultFmt (EslCodeFmt)

  open CodeFmt

  let trace_expr code (lvl : int) (e : Expr.t) ((heap, v) : heapval) : unit =
    if log_level lvl && log_expr e then
      Log.stderr "%a@." (pp_expr code heap) (lvl, e, v)

  let trace_stmt code (lvl : int) (s : Stmt.t) : unit =
    Step.trace_stmt code lvl s

  let trace_restore (lvl : int) (f : Func.t) : unit = Step.trace_restore lvl f

  let trace_call code (lvl : int) (f : Func.t) (s : Stmt.t) : unit =
    Step.trace_call code lvl f s

  let trace_return (lvl : int) (f : Func.t) (s : Stmt.t) (hv : heapval) : unit =
    Step.trace_return lvl f s hv
end

module Core : M = struct
  open DefaultFmt (CeslCodeFmt)

  open CodeFmt

  let trace_expr code (lvl : int) (e : Expr.t) ((heap, v) : heapval) : unit =
    if log_level lvl && log_expr e then
      Log.stderr "%a@." (pp_expr code heap) (lvl, e, v)

  let trace_stmt code (lvl : int) (s : Stmt.t) : unit =
    if log_level lvl && log_stmt s then Log.stderr "%a@." (pp_stmt code) (lvl, s)

  let trace_restore (lvl : int) (f : Func.t) : unit =
    match lvl with
    | -1 -> Log.stderr "%a@." (pp_func "starting on function") (0, f)
    | _ ->
      if log_level lvl then
        Log.stderr "%a@." (pp_func "returning to function") (lvl, f)

  let trace_call _ (lvl : int) (f : Func.t) (_ : Stmt.t) : unit =
    if log_level lvl then
      Log.stderr "%a@." (pp_func "entering function") (lvl, f)

  let trace_return (lvl : int) (f : Func.t) (_ : Stmt.t) (_ : heapval) : unit =
    if log_level lvl then Log.stderr "%a@." (pp_func "exiting function") (lvl, f)
end
