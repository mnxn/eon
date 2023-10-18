type position = Lexing.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving show { with_path = false }]

type range = position * position [@@deriving show]

type printable =
  | P :
      { value : 'a
      ; pp : Format.formatter -> 'a -> unit
      }
      -> printable

let box_pp value pp = P { value; pp }

let pp_printable ppf (P { value; pp }) = pp ppf value

type type_error =
  | Type_not_in_scope of string
  | Value_not_in_scope of string
  | Record_missing of string
  | Type_mismatch of
      { expected : printable
      ; actual : printable
      }
  | Not_numeric of printable
  | Not_pointer of printable
  | Not_array of printable
  | Empty_array
  | Not_record of printable
  | No_field of
      { actual : printable
      ; field : string
      }
  | Initializer_no_field of
      { expected : printable
      ; field : string
      }
  | Initializer_missing_field_value of
      { expected : printable
      ; field : string
      }
  | Initializer_duplicate_field of
      { expected : printable
      ; field : string
      }
  | Not_function of printable
  | Argument_count_mismatch of
      { expected : int
      ; actual : int
      }
  | If_branch_mismatch of
      { true_branch : printable
      ; false_branch : printable
      }

type runtime_error =
  | Undefined_value of string
  | Zero_division
  | Value_shape_mismatch of
      { expected : printable
      ; actual : printable
      }
  | Value_argument_count_mismatch of
      { expected : int
      ; actual : int
      }

type error =
  | Lexer_error of range
  | Parser_error of range
  | Type_error of
      { type_error : type_error
      ; range : range
      }
  | Runtime_error of
      { runtime_error : runtime_error
      ; range : range option
      }

let range = function
  | Lexer_error r -> Some r
  | Parser_error r -> Some r
  | Type_error { range; _ } -> Some range
  | Runtime_error { range; _ } -> range

let column { Lexing.pos_cnum; pos_bol; _ } = pos_cnum - pos_bol + 1

let pp_details ppf = function
  | Lexer_error _ -> Format.fprintf ppf "Lexing error"
  | Parser_error _ -> Format.fprintf ppf "Parsing error"
  | Type_error { type_error; _ } ->
    Format.fprintf ppf "Type error: ";
    begin
      match type_error with
      | Type_not_in_scope name -> Format.fprintf ppf "type %s is not in scope" name
      | Value_not_in_scope name -> Format.fprintf ppf "value %s is not in scope" name
      | Record_missing name -> Format.fprintf ppf "missing definition for record %s" name
      | Type_mismatch { expected; actual } ->
        Format.fprintf
          ppf
          "expected type %a, but got %a"
          pp_printable
          expected
          pp_printable
          actual
      | Not_numeric actual ->
        Format.fprintf
          ppf
          "expected numeric type (int or float), but got %a"
          pp_printable
          actual
      | Not_pointer actual ->
        Format.fprintf ppf "expected pointer type, but got %a" pp_printable actual
      | Not_array actual ->
        Format.fprintf ppf "expected array type, but got %a" pp_printable actual
      | Empty_array -> Format.fprintf ppf "cannot infer type of empty array"
      | Not_record actual ->
        Format.fprintf ppf "expected record type, but got %a" pp_printable actual
      | No_field { actual; field } ->
        Format.fprintf
          ppf
          "value of record type %a has no field %s"
          pp_printable
          actual
          field
      | Initializer_no_field { expected; field } ->
        Format.fprintf
          ppf
          "initializer for record type %a has a value for nonexistent field %s"
          pp_printable
          expected
          field
      | Initializer_missing_field_value { expected; field } ->
        Format.fprintf
          ppf
          "initializer for record type %a is missing a value for field %s"
          pp_printable
          expected
          field
      | Initializer_duplicate_field { expected; field } ->
        Format.fprintf
          ppf
          "initializer for record type %a has a duplicate value for field %s"
          pp_printable
          expected
          field
      | Not_function actual ->
        Format.fprintf ppf "expected function type, but got %a" pp_printable actual
      | Argument_count_mismatch { expected; actual } ->
        Format.fprintf ppf "expected %d arguments, but got %d" expected actual
      | If_branch_mismatch { true_branch; false_branch } ->
        Format.fprintf
          ppf
          "true branch has type %a, but false branch has type %a"
          pp_printable
          true_branch
          pp_printable
          false_branch
    end
  | Runtime_error { runtime_error; _ } -> begin
    Format.fprintf ppf "Runtime error (FATAL): ";
    match runtime_error with
    | Undefined_value name -> Format.fprintf ppf "value %s is undefined" name
    | Zero_division -> Format.fprintf ppf "division by zero"
    | Value_shape_mismatch { expected; actual } ->
      Format.fprintf
        ppf
        "expected %a, bot got %a"
        pp_printable
        expected
        pp_printable
        actual
    | Value_argument_count_mismatch { expected; actual } ->
      Format.fprintf ppf "expected %d arguments, but got %d" expected actual
  end

let pp_error_code (gen : char Gen.t) ppf (start_pos, end_pos) =
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
  done

let pp_error gen ppf e =
  Option.iter (pp_error_code gen ppf) (range e);
  Format.fprintf ppf "%a@." pp_details e
