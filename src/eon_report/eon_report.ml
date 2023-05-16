type position =
  { line : int
  ; column : int
  ; offset : int
  }

type range =
  { file_name : string
  ; start_pos : position
  ; end_pos : position
  }

type error =
  | Lexer_error of range
  | Parser_error of range

let range = function
  | Lexer_error r -> r
  | Parser_error r -> r

let message = function
  | Lexer_error _ -> "Lexing error"
  | Parser_error _ -> "Parsing error"

let rec skip_n_lines ic n =
  if n > 0 then begin
    ignore (In_channel.input_line ic);
    skip_n_lines ic (n - 1)
  end

let pp_error file ppf e =
  let { file_name; start_pos; end_pos } = range e in
  Format.fprintf ppf "File \"%s\", " file_name;

  if start_pos.line = end_pos.line then
    Format.fprintf ppf "line %d, " start_pos.line
  else
    Format.fprintf ppf "lines %d-%d, " start_pos.line end_pos.line;

  if start_pos.column = end_pos.column then
    Format.fprintf ppf "character %d:\n" start_pos.column
  else
    Format.fprintf ppf "characters %d-%d:\n" start_pos.column end_pos.column;

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
    Format.fprintf ppf "%*d | " max_line_digits line;

    if in_range (In_channel.pos file) then Format.pp_print_string ppf Colors.red;
    while
      match !current with
      | None -> false
      | Some '\n' -> false
      | _ -> true
    do
      let pos = Int64.to_int (In_channel.pos file) in
      if pos - 1 = start_pos.offset then Format.pp_print_string ppf Colors.red;
      Option.iter (Format.pp_print_char ppf) !current;
      if pos = end_pos.offset then Format.pp_print_string ppf Colors.clear;

      current := In_channel.input_char file
    done;
    if in_range (In_channel.pos file) then Format.pp_print_string ppf Colors.clear;

    Format.pp_print_newline ppf ()
  done;
  In_channel.seek file old_pos;

  Format.fprintf ppf "Error: %s\n" (message e)
