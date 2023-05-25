let run filename =
  let file = In_channel.open_bin filename in
  let lexbuf = Sedlexing.Utf8.from_channel file in
  Sedlexing.set_filename lexbuf filename;
  begin
    match Result.bind (Eon_parser.parse Eon_parser.lex lexbuf) Eon_typechecker.check with
    | Ok cprogram -> print_endline (Eon_typechecker.show_cprogram cprogram)
    | Error e -> Format.eprintf "%a" (Eon_report.pp_error file) e
  end;
  close_in file
