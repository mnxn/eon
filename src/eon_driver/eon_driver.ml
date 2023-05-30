let with_lexbuf ~action ~ok ?error filename =
  let file =
    if filename = "-" then
      stdin
    else
      In_channel.open_bin filename
  in
  let lexbuf = Sedlexing.Utf8.from_channel file in
  Sedlexing.set_filename lexbuf filename;
  let error =
    Option.value error ~default:(Format.eprintf "%a" (Eon_report.pp_error file))
  in
  Result.fold (action lexbuf) ~ok ~error;
  if file <> stdin then close_in file

let lex filename =
  let action = Eon_parser.lex_all in
  let ok = List.iter (Format.printf "%a@." Eon_parser.pp_token) in
  with_lexbuf filename ~action ~ok

let parse filename =
  let action = Eon_parser.parse Eon_parser.lex in
  let ok = Format.printf "%a@." Eon_parser.Parsetree.pp_pprogram in
  with_lexbuf filename ~action ~ok

let check filename =
  let action lexbuf =
    Result.bind (Eon_parser.parse Eon_parser.lex lexbuf) Eon_typechecker.check
  in
  let ok = Format.printf "%a@." Eon_typechecker.Typedtree.pp_cprogram in
  with_lexbuf filename ~action ~ok

let run = function
  | [] -> prerr_endline "missing argument"
  | [ ("lex" | "parse" | "check") ] -> prerr_endline "missing filename"
  | [ filename ] -> check filename
  | [ "lex"; filename ] -> lex filename
  | [ "parse"; filename ] -> parse filename
  | [ "check"; filename ] -> check filename
  | command :: _ -> Printf.eprintf "unknown command: %s\n" command
