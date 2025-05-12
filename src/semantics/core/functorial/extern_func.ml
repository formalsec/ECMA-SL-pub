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

(* This code was based on: https://github.com/OCamlPro/owi/blob/main/src/intf/func_intf.ml *)
open Prelude

type 'a err =
  [ `Abort of string
  | `Assert_failure of EslSyntax.Stmt.t * 'a
  | `Eval_failure of 'a
  | `Exec_failure of 'a
  | `ReadFile_failure of 'a
  | `Failure of string
  ]

module type Monad_type = sig
  type 'a t
end

module type T = sig
  type value

  type 'a choice

  type nonrec err = value err

  type _ atype =
    | UArg : 'a atype -> (unit -> 'a) atype
    | Arg : 'a atype -> (value -> 'a) atype
    | Res : (value, err) Result.t choice atype

  type _ func_type = Func : 'a atype -> 'a func_type

  type extern_func = Extern_func : 'a func_type * 'a -> extern_func
end

module Make (Value : Value_intf.T) (M : Monad_type) = struct
  type value = Value.value

  type 'a choice = 'a M.t

  type nonrec err = value err

  type _ atype =
    | UArg : 'a atype -> (unit -> 'a) atype
    | Arg : 'a atype -> (value -> 'a) atype
    | Res : (value, err) Result.t choice atype

  type _ func_type = Func : 'a atype -> 'a func_type

  type extern_func = Extern_func : 'a func_type * 'a -> extern_func
end
