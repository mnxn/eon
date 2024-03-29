let ( let* ) r f = Result.bind r f

let with_lexbuf ~action ~ok ?error =
  let go filename gen =
    let gen = Gen.Restart.of_gen gen in
    let lexbuf = Sedlexing.Utf8.from_gen (gen ()) in
    Sedlexing.set_filename lexbuf filename;
    let error =
      match error with
      | Some error -> error
      | None -> Format.eprintf "%a" (Eon_report.pp_error (gen ()))
    in
    Result.fold (action lexbuf) ~ok ~error
  in
  function
  | "-" -> go "<stdin>" (fun () -> In_channel.input_char stdin)
  | filename -> Gen.IO.with_in filename (go filename)

let lex filename =
  let action = Eon_parser.lex_all in
  let ok = List.iter (Format.printf "%a@." Eon_parser.pp_token) in
  with_lexbuf filename ~action ~ok

let parse filename =
  let action = Eon_parser.(parse_program lex) in
  let ok = Format.printf "%a@." Eon_parser.Parsetree.pp_pprogram in
  with_lexbuf filename ~action ~ok

let parse_expression filename =
  let action = Eon_parser.(parse_expression lex) in
  let ok = Format.printf "%a@." Eon_parser.Parsetree.pp_pexpression in
  with_lexbuf filename ~action ~ok

let check filename =
  let action lexbuf =
    let* pprogram = Eon_parser.(parse_program lex) lexbuf in
    Eon_typechecker.check pprogram
  in
  let ok = Format.printf "%a@." Eon_typechecker.Typedtree.pp_cprogram in
  with_lexbuf filename ~action ~ok

let check_expression filename =
  let action lexbuf =
    let* pexpression = Eon_parser.(parse_expression lex) lexbuf in
    Eon_typechecker.(check_expression Primitive.env) pexpression
  in
  let ok = Format.printf "%a@." Eon_typechecker.Typedtree.pp_cexpression in
  with_lexbuf filename ~action ~ok

let run filename =
  let action lexbuf =
    let* pprogram = Eon_parser.(parse_program lex) lexbuf in
    let* cprogram = Eon_typechecker.check pprogram in
    Eon_interpreter.(try_eval run Env.empty) cprogram
  in
  let ok = Format.printf "%a@." Eon_interpreter.print_value in
  with_lexbuf filename ~action ~ok

let eval filename =
  let action lexbuf =
    let* pexpression = Eon_parser.(parse_expression lex) lexbuf in
    let* cexpression = Eon_typechecker.(check_expression Primitive.env) pexpression in
    Eon_interpreter.(try_eval eval_expression Env.empty) cexpression
  in
  let ok = Format.printf "%a@." Eon_interpreter.print_value in
  with_lexbuf filename ~action ~ok

let run = function
  | [] -> prerr_endline "missing argument"
  | [ ( "lex"
      | "parse"
      | "check"
      | "run"
      | "parse-expression"
      | "check-expression"
      | "eval" )
    ] -> prerr_endline "missing filename"
  | [ filename ] -> run filename
  | [ "lex"; filename ] -> lex filename
  | [ "parse"; filename ] -> parse filename
  | [ "check"; filename ] -> check filename
  | [ "run"; filename ] -> run filename
  | [ "parse-expression"; filename ] -> parse_expression filename
  | [ "check-expression"; filename ] -> check_expression filename
  | [ "eval"; filename ] -> eval filename
  | command :: _ -> Printf.eprintf "unknown command: %s\n" command
