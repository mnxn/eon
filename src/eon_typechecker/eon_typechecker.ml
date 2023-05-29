open Eon_parser.Parsetree
open Typedtree

type ('parsed, 'typed) check = 'parsed -> ('typed, Eon_report.error) result

let ( let* ) r f = Result.bind r f

let ( let+ ) r f = Result.map f r

let type_error = Error Eon_report.Type_error

module Primitive = struct
  let unit = CPrimitive_type "unit"

  let bool = CPrimitive_type "bool"

  let int = CPrimitive_type "int"

  let float = CPrimitive_type "float"

  let string = CPrimitive_type "string"

  let env =
    Env.add_types
      [ "unit", unit; "bool", bool; "int", int; "float", float; "string", string ]
      Env.empty
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
  | PNamed_type { name; range = _ } -> begin
    match Env.lookup_type name env with
    | Some t -> Ok t
    | None -> type_error
  end
  | PPointer_type { underlying_type; range = _ } ->
    let+ ctype = check_type env underlying_type in
    CPointer_type ctype
  | PArray_type { element_type; range = _ } ->
    let+ ctype = check_type env element_type in
    CArray_type ctype
  | PFunction_type { parameters; return_type; range = _ } ->
    let* parameters = check_parameters parameters in
    let+ return_type = check_type env return_type in
    CFunction_type { parameters; return_type }

let rec check_expression (env : Env.t) : (pexpression, cexpression) check = function
  | PIdentifier { name; range = _ } ->
    let+ ctype =
      begin
        match Env.lookup_value name env with
        | Some t -> Ok t
        | None -> type_error
      end
    in
    CIdentifier { name; ctype }
  | PUnit { range = _ } -> Ok (CUnit { ctype = Primitive.unit })
  | PBoolean { value; range = _ } -> Ok (CBoolean { value; ctype = Primitive.bool })
  | PInteger { value; range = _ } -> Ok (CInteger { value; ctype = Primitive.int })
  | PFloat { value; range = _ } -> Ok (CFloat { value; ctype = Primitive.float })
  | PString { value; range = _ } -> Ok (CString { value; ctype = Primitive.string })
  | PArray { elements = []; range = _ } -> type_error
  | PArray { elements = first :: rest; range = _ } ->
    let* cfirst = check_expression env first in
    let element_type = cexpression_type cfirst in
    let+ crest =
      let f expression = function
        | Ok acc ->
          let* cexpression = check_expression env expression in
          if cexpression_type cexpression = element_type then
            Ok (cexpression :: acc)
          else
            type_error
        | e -> e
      in
      List.fold_right f rest (Ok [])
    in
    let celements = cfirst :: crest in
    CArray { elements = celements; ctype = CArray_type element_type }
  | PRecord { name; fields; range = _ } ->
    let* ctype =
      begin
        match Env.lookup_type name env with
        | Some t -> Ok t
        | None -> type_error
      end
    in
    let* record_fields =
      match ctype with
      | CRecord_type { name = _; fields } -> Ok fields
      | _ -> type_error
    in
    let+ cfields =
      let field_counter = Hashtbl.create 10 in
      let f (name, value) = function
        | Ok acc ->
          let* cvalue = check_expression env value in
          if Option.is_none @@ Hashtbl.find_opt field_counter name then
            type_error
          else begin
            Hashtbl.add field_counter name ();
            match List.assoc_opt name record_fields with
            | Some expected when cexpression_type cvalue = expected ->
              Ok ((name, cvalue) :: acc)
            | _ -> type_error
          end
        | e -> e
      in
      List.fold_right f fields (Ok [])
    in
    CRecord { name; fields = cfields; ctype }
  | PIndex { expression; index; range = _ } ->
    let* cexpression = check_expression env expression in
    let* cindex = check_expression env index in
    begin
      match cexpression_type cexpression with
      | CArray_type element_type ->
        Ok (CIndex { expression = cexpression; index = cindex; ctype = element_type })
      | _ -> type_error
    end
  | PAccess { expression; field; range = _ } ->
    let* cexpression = check_expression env expression in
    begin
      match cexpression_type cexpression with
      | CRecord_type { name = _; fields } -> begin
        match List.assoc_opt field fields with
        | Some field_type ->
          Ok (CAccess { expression = cexpression; field; ctype = field_type })
        | None -> type_error
      end
      | _ -> type_error
    end
  | PAssign { target; source; range = _ } ->
    let* ctarget = check_expression env target in
    let* csource = check_expression env source in
    if cexpression_type ctarget = cexpression_type csource then
      Ok (CAssign { target = ctarget; source = csource; ctype = Primitive.unit })
    else
      type_error
  | PApply { func; arguments; range = _ } ->
    let* cfunc = check_expression env func in
    let* cfunc_params, cfunc_result =
      match cexpression_type cfunc with
      | CFunction_type { parameters; return_type } -> Ok (parameters, return_type)
      | _ -> type_error
    in
    let+ cargs =
      let f expected arg = function
        | Ok acc ->
          let* carg = check_expression env arg in
          let actual = cexpression_type carg in
          if actual = expected then
            Ok (carg :: acc)
          else
            type_error
        | e -> e
      in
      List.fold_right2 f cfunc_params arguments (Ok [])
    in
    CApply { func = cfunc; arguments = cargs; ctype = cfunc_result }
  | PUnary_operator { operator; expression; range = _ } ->
    let* cexpression = check_expression env expression in
    let ctype = cexpression_type cexpression in
    let+ cresult_type =
      match operator with
      | Not ->
        if ctype = Primitive.bool then
          Ok Primitive.bool
        else
          type_error
      | Negate ->
        if ctype = Primitive.int || ctype = Primitive.float then
          Ok ctype
        else
          type_error
      | Address_of -> Ok (CPointer_type ctype)
      | Dereference -> begin
        match ctype with
        | CPointer_type underlying -> Ok underlying
        | _ -> type_error
      end
    in
    CUnary_operator { operator; expression = cexpression; ctype = cresult_type }
  | PBinary_operator { left; operator; right; range = _ } ->
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
          type_error
      | Equal | Not_equal ->
        if cleft_type = cright_type then
          Ok cleft_type
        else
          type_error
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
          type_error
    in
    CBinary_operator { left = cleft; operator; right = cright; ctype = cresult_type }
  | PBlock { body; range = _ } ->
    let+ cblock = check_block env body in
    CBlock cblock
  | PLet { name; value_type = Some value_type; value; range = _ } ->
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
      type_error
  | PLet { name; value_type = None; value; range = _ } ->
    let+ cvalue = check_expression env value in
    let cvalue_type = cexpression_type cvalue in
    CLet { name; value_type = cvalue_type; value = cvalue; ctype = Primitive.unit }
  | PIf { condition; true_branch; false_branch; range = _ } ->
    let* ccondition = check_expression env condition in
    let* () =
      let ctype = cexpression_type ccondition in
      if ctype = Primitive.bool then
        Ok ()
      else
        type_error
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
      type_error
  | PClosure { parameters; return_type; body; range = _ } ->
    let* cparameters =
      let f (name, ptype) = function
        | Ok acc ->
          let* ctype = check_type env ptype in
          Ok ((name, ctype) :: acc)
        | e -> e
      in
      List.fold_right f parameters (Ok [])
    in
    let body_env = Env.add_values cparameters env in
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
          type_error
    in
    let ctype =
      CFunction_type { parameters = List.map snd cparameters; return_type = creturn_type }
    in
    Ok
      (CClosure
         { parameters = cparameters; return_type = creturn_type; body = cbody; ctype })

and check_block (env : Env.t) : (pblock, cblock) check =
  let rec check_statements env : (pexpression list, cexpression list) check = function
    | [] -> Ok []
    | PLet { name; value_type; value; range = _ } :: stats ->
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
            type_error
      in
      let+ cexpressions = check_statements (Env.add_value name cvalue_type env) stats in
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

let rec check_definitions (env : Env.t) : (pdefinition list, cdefinition list) check =
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
      | PFunction { name; parameters; return_type; body; range = _ } ->
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
        let body_env = Env.add_values cparameters env in
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
          Ok (name, `Value ctype, cfunction)
        else
          type_error
      | PType_alias { name; value; range = _ } ->
        let+ ctype = check_type env value in
        name, `Type ctype, CType_alias { name; value = ctype }
      | PType_record { name; fields; range = _ } ->
        let+ cfields = check_fields fields in
        ( name
        , `Type (CRecord_type { name; fields = cfields })
        , CType_record { name; fields = cfields } )
    in
    let new_env =
      match binding with
      | `Type t -> Env.add_type name t env
      | `Value v -> Env.add_value name v env
    in
    let+ crest = check_definitions new_env pdefs in
    cdef :: crest

let check : (pprogram, cprogram) check = check_definitions Primitive.env

module Typedtree = Typedtree
