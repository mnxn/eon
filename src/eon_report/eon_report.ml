type position = Lexing.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving show { with_path = false }]

type range =
  { start_pos : position
  ; end_pos : position
  }
[@@deriving show { with_path = false }]

let pos (start_pos : Lexing.position) =
  let end_pos = { start_pos with pos_cnum = start_pos.pos_cnum + 1 } in
  { start_pos; end_pos }

let loc (start_pos, end_pos) = { start_pos; end_pos }

type error =
  | Lexer_error of range
  | Parser_error of range
  | Type_error

let range = function
  | Lexer_error r -> r
  | Parser_error r -> r
  | Type_error -> { start_pos = Lexing.dummy_pos; end_pos = Lexing.dummy_pos }

let column { Lexing.pos_cnum; pos_bol; _ } = pos_cnum - pos_bol + 1

let message = function
  | Lexer_error _ -> "Lexing error"
  | Parser_error _ -> "Parsing error"
  | Type_error -> "Type error"

let rec skip_n_lines ic n =
  if n > 0 then begin
    ignore (In_channel.input_line ic);
    skip_n_lines ic (n - 1)
  end

let pp_error file ppf e =
  let { start_pos; end_pos } = range e in
  Format.fprintf ppf "File \"%s\", " start_pos.pos_fname;

  if start_pos.pos_lnum = end_pos.pos_lnum then
    Format.fprintf ppf "line %d, " start_pos.pos_lnum
  else
    Format.fprintf ppf "lines %d-%d, " start_pos.pos_lnum end_pos.pos_lnum;

  if column start_pos = column end_pos then
    Format.fprintf ppf "character %d:\n" (column start_pos)
  else
    Format.fprintf ppf "characters %d-%d:\n" (column start_pos) (column end_pos);

  let in_range pos =
    let pos = Int64.to_int pos in
    start_pos.pos_cnum <= pos && pos <= end_pos.pos_cnum
  in
  let max_line = Int.to_string end_pos.pos_lnum in
  let max_line_digits = String.length max_line in

  let old_pos = In_channel.pos file in
  In_channel.seek file 0L;
  skip_n_lines file (start_pos.pos_lnum - 1);
  for line = start_pos.pos_lnum to end_pos.pos_lnum do
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
      if pos - 1 = start_pos.pos_cnum then Format.pp_print_string ppf Colors.red;
      Option.iter (Format.pp_print_char ppf) !current;
      if pos = end_pos.pos_cnum then Format.pp_print_string ppf Colors.clear;

      current := In_channel.input_char file
    done;
    if in_range (In_channel.pos file) then Format.pp_print_string ppf Colors.clear;

    Format.pp_print_newline ppf ()
  done;
  In_channel.seek file old_pos;

  Format.fprintf ppf "Error: %s\n" (message e)
