open Eon_parser.Parsetree
open Typedtree
module Env = Map.Make (String)

type ('parsed, 'typed) check = 'parsed -> ('typed, Eon_report.error) result

let ( let* ) r f = Result.bind r f

let ( let+ ) r f = Result.map f r

type binding =
  | Value of ctype
  | Type of ctype

module Primitive = struct
  let unit = CPrimitive_type "unit"

  let bool = CPrimitive_type "bool"

  let int = CPrimitive_type "int"

  let float = CPrimitive_type "float"

  let string = CPrimitive_type "string"

  let env =
    Env.of_seq
    @@ List.to_seq
         [ "unit", Type unit
         ; "bool", Type bool
         ; "int", Type int
         ; "float", Type float
         ; "string", Type string
         ]
end

let rec check_type env : (ptype, ctype) check =
  let rec check_parameters : (ptype list, ctype list) check = function
    | [] -> Ok []
    | ptype :: ptypes ->
      let* ctype = check_type env ptype in
      let+ ctypes = check_parameters ptypes in
      ctype :: ctypes
  in
  function
  | PNamed_type name -> begin
    match Env.find_opt name env with
    | Some (Type ctype) -> Ok ctype
    | _ -> Error Eon_report.Type_error
  end
  | PPointer_type ptype ->
    let+ ctype = check_type env ptype in
    CPointer_type ctype
  | PArray_type ptype ->
    let+ ctype = check_type env ptype in
    CArray_type ctype
  | PFunction_type { parameters; return_type } ->
    let* parameters = check_parameters parameters in
    let+ return_type = check_type env return_type in
    CFunction_type { parameters; return_type }

let rec check_expression (env : binding Env.t) : (pexpression, cexpression) check =
  let lookup_value_type name =
    match Env.find_opt name env with
    | Some (Value ctype) -> Ok ctype
    | _ -> Error Eon_report.Type_error
  in
  let lookup_type name =
    match Env.find_opt name env with
    | Some (Type ctype) -> Ok ctype
    | _ -> Error Eon_report.Type_error
  in
  function
  | PIdentifier name ->
    let+ ctype = lookup_value_type name in
    CIdentifier { name; ctype }
  | PUnit -> Ok (CUnit { ctype = Primitive.unit })
  | PBoolean value -> Ok (CBoolean { value; ctype = Primitive.bool })
  | PInteger value -> Ok (CInteger { value; ctype = Primitive.int })
  | PFloat value -> Ok (CFloat { value; ctype = Primitive.float })
  | PString value -> Ok (CString { value; ctype = Primitive.string })
  | PArray [] -> Error Type_error
  | PArray (first :: rest) ->
    let* cfirst = check_expression env first in
    let element_type = cexpression_type cfirst in
    let+ crest =
      let f expression = function
        | Ok acc ->
          let* cexpression = check_expression env expression in
          if cexpression_type cexpression = element_type then
            Ok (cexpression :: acc)
          else
            Error Type_error
        | e -> e
      in
      List.fold_right f rest (Ok [])
    in
    let celements = cfirst :: crest in
    CArray { elements = celements; ctype = CArray_type element_type }
  | PRecord { name; fields } ->
    let* ctype = lookup_type name in
    let* record_fields =
      match ctype with
      | CRecord_type { fields; _ } -> Ok fields
      | _ -> Error Eon_report.Type_error
    in
    let+ cfields =
      let field_counter = Hashtbl.create 10 in
      let f (name, value) = function
        | Ok acc ->
          let* cvalue = check_expression env value in
          if Option.is_none @@ Hashtbl.find_opt field_counter name then
            Error Type_error
          else begin
            Hashtbl.add field_counter name ();
            match List.assoc_opt name record_fields with
            | Some expected when cexpression_type cvalue = expected ->
              Ok ((name, cvalue) :: acc)
            | _ -> Error Type_error
          end
        | e -> e
      in
      List.fold_right f fields (Ok [])
    in
    CRecord { name; fields = cfields; ctype }
  | PIndex { expression; index } ->
    let* cexpression = check_expression env expression in
    let* cindex = check_expression env index in
    begin
      match cexpression_type cexpression with
      | CArray_type element_type ->
        Ok (CIndex { expression = cexpression; index = cindex; ctype = element_type })
      | _ -> Error Type_error
    end
  | PAccess { expression; field } ->
    let* cexpression = check_expression env expression in
    begin
      match cexpression_type cexpression with
      | CRecord_type { fields; _ } -> begin
        match List.assoc_opt field fields with
        | Some field_type ->
          Ok (CAccess { expression = cexpression; field; ctype = field_type })
        | None -> Error Type_error
      end
      | _ -> Error Type_error
    end
  | PAssign { target; source } ->
    let* ctarget = check_expression env target in
    let* csource = check_expression env source in
    if cexpression_type ctarget = cexpression_type csource then
      Ok (CAssign { target = ctarget; source = csource; ctype = Primitive.unit })
    else
      Error Type_error
  | PApply { func; arguments } ->
    let* cfunc = check_expression env func in
    let* cfunc_params, cfunc_result =
      match cexpression_type cfunc with
      | CFunction_type { parameters; return_type } -> Ok (parameters, return_type)
      | _ -> Error Eon_report.Type_error
    in
    let+ cargs =
      let f expected arg = function
        | Ok acc ->
          let* carg = check_expression env arg in
          let actual = cexpression_type carg in
          if actual = expected then
            Ok (carg :: acc)
          else
            Error Eon_report.Type_error
        | e -> e
      in
      List.fold_right2 f cfunc_params arguments (Ok [])
    in
    CApply { func = cfunc; arguments = cargs; ctype = cfunc_result }
  | PUnary_operator { operator; expression } ->
    let* cexpression = check_expression env expression in
    let ctype = cexpression_type cexpression in
    let+ cresult_type =
      match operator with
      | Not ->
        if ctype = Primitive.bool then
          Ok Primitive.bool
        else
          Error Eon_report.Type_error
      | Negate ->
        if ctype = Primitive.int || ctype = Primitive.float then
          Ok ctype
        else
          Error Type_error
      | Address_of -> Ok (CPointer_type ctype)
      | Dereference -> begin
        match ctype with
        | CPointer_type underlying -> Ok underlying
        | _ -> Error Type_error
      end
    in
    CUnary_operator { operator; expression = cexpression; ctype = cresult_type }
  | PBinary_operator { left; operator; right } ->
    let* cleft = check_expression env left in
    let* cright = check_expression env right in
    let cleft_type = cexpression_type cleft in
    let cright_type = cexpression_type cright in
    let+ cresult_type =
      match operator with
      | And | Or ->
        if cleft_type = Primitive.bool && cright_type = Primitive.bool then
          Ok Primitive.bool
        else
          Error Eon_report.Type_error
      | Equal | Not_equal ->
        if cleft_type = cright_type then
          Ok cleft_type
        else
          Error Eon_report.Type_error
      | Less
      | Less_equal
      | Greater
      | Greater_equal
      | Add
      | Subtract
      | Multiply
      | Divide
      | Remainder ->
        if
          (cleft_type = Primitive.int && cright_type = Primitive.int)
          || (cleft_type = Primitive.float && cright_type = Primitive.float)
        then
          Ok cleft_type
        else
          Error Eon_report.Type_error
    in
    CBinary_operator { left = cleft; operator; right = cright; ctype = cresult_type }
  | PBlock block ->
    let+ cblock = check_block env block in
    CBlock cblock
  | PLet { name; value_type = Some value_type; value } ->
    let* cvalue = check_expression env value in
    let cvalue_type_actual = cexpression_type cvalue in
    let* cvalue_type_expected = check_type env value_type in
    if cvalue_type_actual = cvalue_type_expected then
      Ok
        (CLet
           { name
           ; value_type = cvalue_type_expected
           ; value = cvalue
           ; ctype = Primitive.unit
           })
    else
      Error Type_error
  | PLet { name; value_type = None; value } ->
    let+ cvalue = check_expression env value in
    let cvalue_type = cexpression_type cvalue in
    CLet { name; value_type = cvalue_type; value = cvalue; ctype = Primitive.unit }
  | PIf { condition; true_branch; false_branch } ->
    let* ccondition = check_expression env condition in
    let* () =
      let ctype = cexpression_type ccondition in
      if ctype = Primitive.bool then
        Ok ()
      else
        Error Eon_report.Type_error
    in
    let* ctrue_branch = check_expression env true_branch in
    let ctrue_type = cexpression_type ctrue_branch in
    let* cfalse_branch = check_expression env false_branch in
    let cfalse_type = cexpression_type ctrue_branch in
    if ctrue_type = cfalse_type then
      Ok
        (CIf
           { condition = ccondition
           ; true_branch = ctrue_branch
           ; false_branch = cfalse_branch
           ; ctype = ctrue_type
           })
    else
      Error Type_error
  | PClosure { parameters; return_type; body } ->
    let* cparameters =
      let f (name, ptype) = function
        | Ok acc ->
          let* ctype = check_type env ptype in
          Ok ((name, ctype) :: acc)
        | e -> e
      in
      List.fold_right f parameters (Ok [])
    in
    let body_env =
      let bindings = List.map (fun (n, p) -> n, Value p) cparameters in
      Env.add_seq (List.to_seq bindings) env
    in
    let* cbody = check_expression body_env body in
    let cbody_type = cexpression_type cbody in
    let* creturn_type =
      match return_type with
      | None -> Ok cbody_type
      | Some return_type ->
        let* creturn_type = check_type env return_type in
        if cbody_type = creturn_type then
          Ok creturn_type
        else
          Error Eon_report.Type_error
    in
    let ctype =
      CFunction_type { parameters = List.map snd cparameters; return_type = creturn_type }
    in
    Ok
      (CClosure
         { parameters = cparameters; return_type = creturn_type; body = cbody; ctype })

and check_block (env : binding Env.t) : (pblock, cblock) check =
  let rec check_statements env : (pexpression list, cexpression list) check = function
    | [] -> Ok []
    | PLet { name; value_type; value } :: stats ->
      let* cexpression = check_expression env value in
      let cvalue_type_actual = cexpression_type cexpression in
      let* cvalue_type =
        match value_type with
        | None -> Ok cvalue_type_actual
        | Some value_type ->
          let* cvalue_type_expected = check_type env value_type in
          if cvalue_type_actual = cvalue_type_expected then
            Ok cvalue_type_expected
          else
            Error Type_error
      in
      let binding = Value cvalue_type in
      let+ cexpressions = check_statements (Env.add name binding env) stats in
      CLet { name; value_type = cvalue_type; value = cexpression; ctype = Primitive.unit }
      :: cexpressions
    | stat :: stats ->
      let* cexpression = check_expression env stat in
      let+ cexpressions = check_statements env stats in
      cexpression :: cexpressions
  in
  function
  | { statements; result } ->
    let* cstatements = check_statements env statements in
    let+ cresult, ctype =
      match result with
      | None -> Ok (None, Primitive.unit)
      | Some result ->
        let+ cexpression = check_expression env result in
        let ctype = cexpression_type cexpression in
        Some cexpression, ctype
    in
    { statements = cstatements; result = cresult; ctype }

let rec check_definitions (env : binding Env.t)
  : (pdefinition list, cdefinition list) check
  =
  let rec check_fields : ((string * ptype) list, (string * ctype) list) check = function
    | [] -> Ok []
    | (name, ptype) :: fields ->
      let* ctype = check_type env ptype in
      let+ rest = check_fields fields in
      (name, ctype) :: rest
  in
  function
  | [] -> Ok []
  | pdef :: pdefs ->
    let* name, binding, cdef =
      match pdef with
      | PFunction { name; parameters; return_type; body } ->
        let* cparameters =
          let f (name, ptype) = function
            | Ok acc ->
              let* ctype = check_type env ptype in
              Ok ((name, ctype) :: acc)
            | e -> e
          in
          List.fold_right f parameters (Ok [])
        in
        let* creturn_type = check_type env return_type in
        let body_env =
          let bindings = List.map (fun (n, p) -> n, Value p) cparameters in
          Env.add_seq (List.to_seq bindings) env
        in
        let* cbody = check_expression body_env body in
        let cbody_type = cexpression_type cbody in
        if cbody_type = creturn_type then
          let cfunction =
            CFunction
              { name; parameters = cparameters; return_type = creturn_type; body = cbody }
          in
          let ctype =
            CFunction_type
              { parameters = List.map snd cparameters; return_type = creturn_type }
          in
          Ok (name, Value ctype, cfunction)
        else
          Error Eon_report.Type_error
      | PType_alias { name; value } ->
        let+ ctype = check_type env value in
        name, Type ctype, CType_alias { name; value = ctype }
      | PType_record { name; fields } ->
        let+ cfields = check_fields fields in
        ( name
        , Type (CRecord_type { name; fields = cfields })
        , CType_record { name; fields = cfields } )
    in
    let+ crest = check_definitions (Env.add name binding env) pdefs in
    cdef :: crest

let check : (pprogram, cprogram) check = check_definitions Primitive.env

module Typedtree = Typedtree
