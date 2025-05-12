open Ecma_sl.String_utils

let () =
  assert (4 = s_len_u "\u{00FF}\u{00FF}\u{00FF}\u{00FF}");
  (* invalid utf-8
            "s_len_u - decimal escape length"  >:: (fun _ ->
               assert 4 (s_len_u "\255\255\255\u{00FF}")
             );
            "s_len_u - unicode as decimal length"  >:: (fun _ ->
               assert 4 (s_len_u "\195\191\255\255\195\191")
             );*)
  assert (String.equal "\u{1234}" (s_nth_u "\u{00FF}\u{00FF}\u{1234}\u{00FF}" 2));
  (* invalid utf-8
            "s_nth_u - decimal and hexadecimal escape nth"  >:: (fun _ ->
              assert "\u{1234}" (s_nth_u "\u{00FF}\255\xe1\x88\xb4\255" 2)
            );*)
  assert (255 = to_char_code_u "\u{00FF}");
  (* invalid utf-8
            "to_char_code_u - decimal escape to int"  >:: (fun _ ->
              assert 255 (to_char_code_u "\255")
            );*)
  assert (255 = to_char_code_u "\195\191");
  assert (4660 = to_char_code_u "\u{1234}");
  assert (4660 = to_char_code_u "\xe1\x88\xb4");
  assert (String.equal "\195\191" (from_char_code_u 255));
  assert (String.equal "\u{1234}" (from_char_code_u 4660));
  assert (
    String.equal "\u{1234}\u{00FF}"
      (s_substr_u "\u{00FF}\u{00FF}\u{1234}\u{00FF}" 2 2) );
  (* invalid utf-8
            "s_substr_u - unicode escape substring 2"  >:: (fun _ ->
               assert "\u{1234}\255" (s_substr_u "\u{00FF}\u{00FF}\xe1\x88\xb4\255" 2 2)
             );*)
  assert (
    String.equal "\u{1234}\u{00FF}"
      (s_substr_u "\u{00FF}\u{00FF}\u{1234}\195\191" 2 2) );
  assert (String.equal "\u{1234}" (utf8decode "\\u{1234}"));
  (* Ocaml does not support high-surrogate code points: http://unicode.org/glossary/#unicode_scalar_value *)
  assert (4660 = to_char_code_u (utf8decode "\\u{1234}"));
  assert (55348 = to_char_code_u (utf8decode "\\u{D834}"));
  assert (String.equal "\x20" (hexdecode "\\x20"));
  assert (String.equal "\u{10FFFF}" (utf8decode "\\u{10FFFF}"));
  assert (1114111 = to_char_code_u (utf8decode "\\u{10FFFF}"));
  assert ("\u{10000}" = utf8decode "\\u{10000}");
  assert (1114111 = 0x10FFFF)
