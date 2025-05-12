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

include Smtml.Solver.Batch (Smtml.Z3_mappings)

let check solver assumptions =
  Logs.debug (fun k ->
    k "@[<hov 1>solver:@ %a@]" Smtml.Expr.pp_list assumptions );
  check solver assumptions

let pp_set fmt v = Smtml.Expr.Set.pretty ~pp_sep:Fmt.sp Smtml.Expr.pp fmt v

let check_set solver assumptions =
  Logs.debug (fun k -> k "@[<hov 1>solver:@ %a@]" pp_set assumptions);
  check_set solver assumptions
