type position = Lexing.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving show { with_path = false }]

type range = position * position [@@deriving show]

type error =
  | Lexer_error of range
  | Parser_error of range
  | Type_error of range

let range = function
  | Lexer_error r -> r
  | Parser_error r -> r
  | Type_error r -> r

let column { Lexing.pos_cnum; pos_bol; _ } = pos_cnum - pos_bol + 1

let prefix = function
  | Lexer_error _ -> "Lexing error"
  | Parser_error _ -> "Parsing error"
  | Type_error _ -> "Type error"

let details = function
  | Lexer_error _ -> None
  | Parser_error _ -> None
  | Type_error _ -> None

let pp_error (gen : char Gen.t) ppf e =
  let start_pos, end_pos = range e in
  Format.fprintf ppf "File \"%s\", " start_pos.pos_fname;

  if start_pos.pos_lnum = end_pos.pos_lnum then
    Format.fprintf ppf "line %d, " start_pos.pos_lnum
  else
    Format.fprintf ppf "lines %d-%d, " start_pos.pos_lnum end_pos.pos_lnum;

  if column start_pos = column end_pos then
    Format.fprintf ppf "character %d:\n" (column start_pos)
  else
    Format.fprintf ppf "characters %d-%d:\n" (column start_pos) (column end_pos);

  let in_range pos = start_pos.pos_cnum <= pos && pos <= end_pos.pos_cnum in
  let max_line = Int.to_string end_pos.pos_lnum in
  let max_line_digits = String.length max_line in

  let gen = Gen.drop start_pos.pos_bol gen in
  let pos = ref start_pos.pos_bol in
  for line = start_pos.pos_lnum to end_pos.pos_lnum do
    let current = ref (Gen.get gen) in
    incr pos;
    Format.fprintf ppf "%*d | " max_line_digits line;

    if in_range !pos then Format.pp_print_string ppf Colors.red;
    while
      match !current with
      | None -> false
      | Some '\n' -> false
      | _ -> true
    do
      if !pos - 1 = start_pos.pos_cnum then Format.pp_print_string ppf Colors.red;
      Option.iter (Format.pp_print_char ppf) !current;
      if !pos = end_pos.pos_cnum || start_pos = end_pos then
        Format.pp_print_string ppf Colors.clear;

      current := Gen.get gen;
      incr pos
    done;
    if in_range !pos then Format.pp_print_string ppf Colors.clear;

    Format.pp_print_newline ppf ()
  done;

  match prefix e, details e with
  | p, Some d -> Format.fprintf ppf "%s: %s@." p d
  | p, None -> Format.fprintf ppf "%s@." p
