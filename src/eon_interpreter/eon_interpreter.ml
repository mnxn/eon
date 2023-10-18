open Eon_typechecker
open Eon_parser
open Eon_report
module Env = Map.Make (String)

type value =
  | Unit
  | Boolean of bool
  | Integer of int64
  | Float of float
  | String of string
  | Array of value array
  | Record of (string, value) Hashtbl.t
  | Box of { mutable value : value }
  | Closure of
      { parameters : string array
      ; body : Typedtree.cexpression
      ; env : value Env.t Lazy.t
      }

let rec print_value ppf = function
  | Unit -> Format.fprintf ppf "()"
  | Boolean b -> Format.fprintf ppf "%b" b
  | Integer i -> Format.fprintf ppf "%Ld" i
  | Float f -> Format.fprintf ppf "%f" f
  | String s -> Format.fprintf ppf "%s" s
  | Array a ->
    let length = Array.length a in
    let print_array_element i value =
      print_value ppf value;
      if i < length - 1 then Format.fprintf ppf ", "
    in
    Format.fprintf ppf "[ ";
    Array.iteri print_array_element a;
    Format.fprintf ppf " ]"
  | Record r ->
    let length = Hashtbl.length r in
    let print_record_element name value i =
      Format.fprintf ppf "%s = %a" name print_value value;
      if i < length - 1 then Format.fprintf ppf ",";
      i + 1
    in
    Format.fprintf ppf "{ ";
    ignore @@ Hashtbl.fold print_record_element r 0;
    Format.fprintf ppf " }"
  | Box { value } -> Format.fprintf ppf "&%a" print_value value
  | Closure { parameters; _ } ->
    Format.fprintf ppf "<closure/%d>" (Array.length parameters)

let rec equal_value l r =
  match l, r with
  | Unit, Unit -> true
  | Boolean l, Boolean r -> Bool.equal l r
  | Integer l, Integer r -> Int64.equal l r
  | Float l, Float r -> Float.equal l r
  | String l, String r -> String.equal l r
  | Array l, Array r -> Array.length l = Array.length r && Array.for_all2 equal_value l r
  | Record l, Record r ->
    let l_bindings = sort_bindings l in
    let r_bindings = sort_bindings r in
    Array.length l_bindings = Array.length r_bindings
    && Array.for_all2 equal_bindings l_bindings r_bindings
  | Box { value = l }, Box { value = r } -> equal_value l r
  | Closure _, Closure _ -> l == r
  | _ -> false

and sort_bindings tbl =
  let arr = tbl |> Hashtbl.to_seq |> Array.of_seq in
  Array.sort (fun (l, _) (r, _) -> String.compare l r) arr;
  arr

and equal_bindings (l_name, l_value) (r_name, r_value) =
  String.equal l_name r_name && equal_value l_value r_value

type shape =
  | Unit_shape
  | Boolean_shape
  | Integer_shape
  | Float_shape
  | Integer_or_float_shape
  | String_shape
  | Array_shape
  | Record_shape
  | Box_shape
  | Closure_shape

let print_shape ppf shape =
  Format.fprintf ppf
  @@
  match shape with
  | Unit_shape -> "a unit value"
  | Boolean_shape -> "a boolean value"
  | Integer_shape -> "an integer value"
  | Float_shape -> "a float value"
  | Integer_or_float_shape -> "an integer or float value"
  | String_shape -> "a string value"
  | Array_shape -> "an array value"
  | Record_shape -> "a record value"
  | Box_shape -> "a boxed value"
  | Closure_shape -> "a closure value"

let value_shape = function
  | Unit -> Unit_shape
  | Boolean _ -> Boolean_shape
  | Integer _ -> Integer_shape
  | Float _ -> Float_shape
  | String _ -> String_shape
  | Array _ -> Array_shape
  | Record _ -> Record_shape
  | Box _ -> Box_shape
  | Closure _ -> Closure_shape

exception
  Runtime_exception of
    { runtime_error : Eon_report.runtime_error
    ; range : Eon_report.range option
    }

let raise_shape_mismatch ?range expected_shape actual_value =
  let runtime_error =
    Value_shape_mismatch
      { expected = box_pp expected_shape print_shape
      ; actual = box_pp (value_shape actual_value) print_shape
      }
  in
  raise @@ Runtime_exception { runtime_error; range }

let lookup_value env ?range name =
  match Env.find_opt name env with
  | Some v -> v
  | None -> raise @@ Runtime_exception { runtime_error = Undefined_value name; range }

let ( let* ) r f = Result.bind r f

let ( let+ ) r f = Result.map f r

let rec eval_expression env : Typedtree.cexpression -> value = function
  | CIdentifier { name; range; _ } -> lookup_value ~range env name
  | CUnit _ -> Unit
  | CBoolean { value; _ } -> Boolean value
  | CInteger { value; _ } -> Integer value
  | CFloat { value; _ } -> Float value
  | CString { value; _ } -> String value
  | CArray { elements; _ } ->
    let values = List.map (eval_expression env) elements in
    Array (Array.of_list values)
  | CRecord { fields; _ } ->
    let values =
      fields
      |> List.map (fun (name, cexpr) -> name, eval_expression env cexpr)
      |> List.to_seq
      |> Hashtbl.of_seq
    in
    Record values
  | CIndex { expression; index; _ } ->
    let expression_value = eval_expression env expression in
    let index_value = eval_expression env index in
    begin
      match expression_value, index_value with
      | Array a, Integer i -> a.(Int64.to_int i)
      | Array _, actual ->
        raise_shape_mismatch
          Integer_shape
          actual
          ~range:(Typedtree.cexpression_range index)
      | actual, _ ->
        raise_shape_mismatch
          Array_shape
          actual
          ~range:(Typedtree.cexpression_range expression)
    end
  | CAccess { expression; field; _ } ->
    let expression_value = eval_expression env expression in
    begin
      match expression_value with
      | Record r -> Hashtbl.find r field
      | actual ->
        raise_shape_mismatch
          Record_shape
          actual
          ~range:(Typedtree.cexpression_range expression)
    end
  | CAssign { target; source; _ } ->
    let target_value = eval_expression env target in
    let source_value = eval_expression env source in
    begin
      match target_value with
      | Box b ->
        b.value <- source_value;
        Unit
      | actual ->
        raise_shape_mismatch Box_shape actual ~range:(Typedtree.cexpression_range target)
    end
  | CApply { func; arguments; _ } ->
    let func_value = eval_expression env func in
    let arguments = Array.of_list arguments in
    begin
      match func_value with
      | Closure { parameters; _ } when Array.length parameters <> Array.length arguments
        ->
        let runtime_error =
          Value_argument_count_mismatch
            { expected = Array.length parameters; actual = Array.length arguments }
        in
        raise
        @@ Runtime_exception
             { runtime_error; range = Some (Typedtree.cexpression_range func) }
      | Closure { parameters; body; env = (lazy body_env) } ->
        let bindings =
          Array.to_seq
          @@ Array.map2 (fun p a -> p, eval_expression env a) parameters arguments
        in
        eval_expression (Env.add_seq bindings body_env) body
      | actual ->
        raise_shape_mismatch
          Closure_shape
          actual
          ~range:(Typedtree.cexpression_range func)
    end
  | CUnary_operator { operator; expression; _ } ->
    let expression_value = eval_expression env expression in
    begin
      match operator with
      | Not -> begin
        match expression_value with
        | Boolean b -> Boolean (not b)
        | actual ->
          raise_shape_mismatch
            Boolean_shape
            actual
            ~range:(Typedtree.cexpression_range expression)
      end
      | Parsetree.Negate -> begin
        match expression_value with
        | Integer i -> Integer (Int64.neg i)
        | Float f -> Float (Float.neg f)
        | actual ->
          raise_shape_mismatch
            Integer_or_float_shape
            actual
            ~range:(Typedtree.cexpression_range expression)
      end
      | Parsetree.Address_of -> Box { value = expression_value }
      | Parsetree.Dereference -> begin
        match expression_value with
        | Box { value } -> value
        | actual ->
          raise_shape_mismatch
            Box_shape
            actual
            ~range:(Typedtree.cexpression_range expression)
      end
    end
  | CBinary_operator { left; operator; right; _ } ->
    let left_value = eval_expression env left in
    let right_value = lazy (eval_expression env right) in
    let compare_function f_int f_float =
      match left_value, Lazy.force right_value with
      | Integer l, Integer r -> Boolean (f_int l r)
      | Integer _, actual ->
        raise_shape_mismatch
          Integer_shape
          actual
          ~range:(Typedtree.cexpression_range right)
      | Float l, Float r -> Boolean (f_float l r)
      | Float _, actual ->
        raise_shape_mismatch Float_shape actual ~range:(Typedtree.cexpression_range right)
      | actual, _ ->
        raise_shape_mismatch
          Integer_or_float_shape
          actual
          ~range:(Typedtree.cexpression_range left)
    in
    let arithmetic_function f_int f_float =
      match left_value, Lazy.force right_value with
      | Integer l, Integer r -> Integer (f_int l r)
      | Integer _, actual ->
        raise_shape_mismatch
          Integer_shape
          actual
          ~range:(Typedtree.cexpression_range right)
      | Float l, Float r -> Float (f_float l r)
      | Float _, actual ->
        raise_shape_mismatch Float_shape actual ~range:(Typedtree.cexpression_range right)
      | actual, _ ->
        raise_shape_mismatch
          Integer_or_float_shape
          actual
          ~range:(Typedtree.cexpression_range left)
    in
    begin
      match operator with
      | And -> begin
        match left_value with
        | Boolean false -> Boolean false
        | Boolean true -> Lazy.force right_value
        | actual ->
          raise_shape_mismatch
            Boolean_shape
            actual
            ~range:(Typedtree.cexpression_range left)
      end
      | Or -> begin
        match left_value with
        | Boolean true -> Boolean true
        | Boolean false -> Lazy.force right_value
        | actual ->
          raise_shape_mismatch
            Boolean_shape
            actual
            ~range:(Typedtree.cexpression_range left)
      end
      | Equal ->
        let right_forced = Lazy.force right_value in
        if value_shape left_value = value_shape right_forced then
          Boolean (equal_value left_value right_forced)
        else
          raise_shape_mismatch
            (value_shape left_value)
            (Lazy.force right_value)
            ~range:(Typedtree.cexpression_range right)
      | Not_equal ->
        let right_forced = Lazy.force right_value in
        if value_shape left_value = value_shape right_forced then
          Boolean (not @@ equal_value left_value right_forced)
        else
          raise_shape_mismatch
            (value_shape left_value)
            right_forced
            ~range:(Typedtree.cexpression_range right)
      | Less -> compare_function ( < ) ( < )
      | Less_equal -> compare_function ( <= ) ( <= )
      | Greater -> compare_function ( > ) ( > )
      | Greater_equal -> compare_function ( >= ) ( >= )
      | Add -> arithmetic_function Int64.add Float.add
      | Subtract -> arithmetic_function Int64.sub Float.sub
      | Multiply -> arithmetic_function Int64.mul Float.mul
      | Divide ->
        let int64_div l = function
          | 0L ->
            raise
            @@ Runtime_exception
                 { runtime_error = Zero_division
                 ; range = Some (Typedtree.cexpression_range right)
                 }
          | r -> Int64.div l r
        in
        let float_div l = function
          | 0. ->
            raise
            @@ Runtime_exception
                 { runtime_error = Zero_division
                 ; range = Some (Typedtree.cexpression_range right)
                 }
          | r -> Float.div l r
        in
        arithmetic_function int64_div float_div
      | Remainder -> arithmetic_function Int64.rem Float.rem
    end
  | CBlock { block; range } -> eval_block env range block
  | CLet { value; _ } ->
    ignore @@ eval_expression env value;
    Unit
  | CIf { condition; true_branch; false_branch; _ } ->
    let condition_value = eval_expression env condition in
    begin
      match condition_value with
      | Boolean true -> eval_expression env true_branch
      | Boolean false -> eval_expression env false_branch
      | actual ->
        raise_shape_mismatch
          Boolean_shape
          actual
          ~range:(Typedtree.cexpression_range condition)
    end
  | CClosure { parameters; body; _ } ->
    Closure { parameters = Array.of_list (List.map fst parameters); body; env = lazy env }

and eval_block env range = function
  | { statements = []; result = None; _ } -> Unit
  | { statements = []; result = Some result; _ } -> eval_expression env result
  | { statements = CLet { name; value = let_value; _ } :: rest; _ } as block ->
    let value = eval_expression env let_value in
    eval_block (Env.add name value env) range @@ { block with statements = rest }
  | { statements = statement :: rest; _ } as block ->
    ignore @@ eval_expression env statement;
    eval_block env range @@ { block with statements = rest }

let rec eval_definitions env : Typedtree.cdefinition list -> value Env.t = function
  | [] -> env
  | (CType_alias _ | CType_record _) :: rest -> eval_definitions env rest
  | CFunction { name; parameters; body; _ } :: rest ->
    let rec closure =
      Closure
        { parameters = Array.of_list @@ List.map fst parameters
        ; body
        ; env = lazy (Env.add name closure env)
        }
    in
    eval_definitions (Env.add name closure env) rest

let run env cprogram =
  let env = eval_definitions env cprogram in
  match lookup_value env "main" with
  | Closure { parameters = [||]; body; env = (lazy env) } -> eval_expression env body
  | Closure { parameters; _ } ->
    let runtime_error =
      Value_argument_count_mismatch { expected = 0; actual = Array.length parameters }
    in
    raise @@ Runtime_exception { runtime_error; range = None }
  | actual -> raise_shape_mismatch Closure_shape actual

let try_eval eval env x =
  try Ok (eval env x) with
  | Runtime_exception { runtime_error; range } ->
    Error (Runtime_error { runtime_error; range })
