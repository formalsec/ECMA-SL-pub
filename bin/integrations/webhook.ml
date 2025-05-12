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

open Cohttp
open Cohttp_lwt_unix

let headers =
  let headers = Header.init () in
  Header.add_list headers
    [ ("Content-type", "application/json"); ("User-Agent", "Ecmabot/1.0") ]

let default_slack_mrkdwn title body =
  `Assoc
    [ ( "blocks"
      , `List
          [ `Assoc
              [ ("type", `String "header")
              ; ( "text"
                , `Assoc
                    [ ("type", `String "plain_text")
                    ; ("text", `String title)
                    ; ("emoji", `Bool true)
                    ] )
              ]
          ; `Assoc
              [ ("type", `String "section")
              ; ( "text"
                , `Assoc [ ("type", `String "mrkdwn"); ("text", `String body) ]
                )
              ]
          ] )
    ]

let url_of_string str = Uri.of_string str

let post url body =
  let body = Cohttp_lwt.Body.of_string (Yojson.to_string body) in
  Client.post ~body ~headers url

(** Sends POST and forgets about it *)
let post_and_forget url body =
  let open Lwt.Syntax in
  let+ _ = post url body in
  ()
