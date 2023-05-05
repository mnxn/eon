let rec skip_n_lines ic n =
  if n > 0 then begin
    ignore (In_channel.input_line ic);
    skip_n_lines ic (n - 1)
  end

let print_error_range message file { Eon_parser.file_name; start_pos; end_pos } =
  Printf.eprintf "File \"%s\", " file_name;

  if start_pos.line = end_pos.line then
    Printf.eprintf "line %d, " start_pos.line
  else
    Printf.eprintf "lines %d-%d, " start_pos.line end_pos.line;

  if start_pos.column = end_pos.column then
    Printf.eprintf "character %d:\n" start_pos.column
  else
    Printf.eprintf "characters %d-%d:\n" start_pos.column end_pos.column;

  let in_range pos =
    let pos = Int64.to_int pos in
    start_pos.offset <= pos && pos <= end_pos.offset
  in
  let max_line = Int.to_string end_pos.line in
  let max_line_digits = String.length max_line in

  let old_pos = In_channel.pos file in
  In_channel.seek file 0L;
  skip_n_lines file (start_pos.line - 1);
  for line = start_pos.line to end_pos.line do
    let current = ref (In_channel.input_char file) in
    Printf.eprintf "%*d | " max_line_digits line;

    if in_range (In_channel.pos file) then prerr_string Colors.red;
    while
      match !current with
      | None -> false
      | Some '\n' -> false
      | _ -> true
    do
      let pos = Int64.to_int (In_channel.pos file) in
      if pos - 1 = start_pos.offset then prerr_string Colors.red;
      Option.iter prerr_char !current;
      if pos = end_pos.offset then prerr_string Colors.clear;

      current := In_channel.input_char file
    done;
    if in_range (In_channel.pos file) then prerr_string Colors.clear;

    prerr_newline ()
  done;
  In_channel.seek file old_pos;

  Printf.eprintf "Error: %s\n" message

let run filename =
  let file = In_channel.open_bin filename in
  let lexbuf = Sedlexing.Utf8.from_channel file in
  Sedlexing.set_filename lexbuf filename;
  begin
    match Eon_parser.parse Eon_parser.lex lexbuf with
    | Ok program -> print_endline (Eon_parser.show_program program)
    | Error range -> print_error_range "Syntax error" file range
  end;
  close_in file
