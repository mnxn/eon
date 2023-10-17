open Eon_parser.Parsetree
open Typedtree
open Eon_report

type ('parsed, 'typed) check = 'parsed -> ('typed, Eon_report.error) result

let ( let* ) r f = Result.bind r f

let ( let+ ) r f = Result.map f r

let type_error range type_error = Error (Type_error { type_error; range })

let box_ctype ctype = box_pp ctype print_ctype

let type_mismatch expected actual =
  Type_mismatch { expected = box_ctype expected; actual = box_ctype actual }

module Primitive = struct
  let unit = CPrimitive_type "unit"

  let bool = CPrimitive_type "bool"

  let int = CPrimitive_type "int"

  let float = CPrimitive_type "float"

  let string = CPrimitive_type "string"

  let env =
    Env.add_all
      Type
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
  | PNamed_type { name; range } -> begin
    match Env.lookup Type name env with
    | Some t -> Ok t
    | None -> type_error range @@ Type_not_in_scope name
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
  | PIdentifier { name; range } ->
    let+ ctype =
      begin
        match Env.lookup Value name env with
        | Some t -> Ok t
        | None -> type_error range @@ Value_not_in_scope name
      end
    in
    CIdentifier { name; ctype }
  | PUnit { range = _ } -> Ok (CUnit { ctype = Primitive.unit })
  | PBoolean { value; range = _ } -> Ok (CBoolean { value; ctype = Primitive.bool })
  | PInteger { value; range = _ } -> Ok (CInteger { value; ctype = Primitive.int })
  | PFloat { value; range = _ } -> Ok (CFloat { value; ctype = Primitive.float })
  | PString { value; range = _ } -> Ok (CString { value; ctype = Primitive.string })
  | PArray { elements = []; range } -> type_error range Empty_array
  | PArray { elements = first :: rest; range = _ } ->
    let* cfirst = check_expression env first in
    let element_type = cexpression_type cfirst in
    let+ crest =
      let f pexpression = function
        | Ok acc ->
          let* cexpr = check_expression env pexpression in
          let cexpr_type = cexpression_type cexpr in
          if cexpr_type = element_type then
            Ok (cexpr :: acc)
          else
            type_error (pexpression_range pexpression)
            @@ type_mismatch element_type cexpr_type
        | e -> e
      in
      List.fold_right f rest (Ok [])
    in
    let celements = cfirst :: crest in
    CArray { elements = celements; ctype = CArray_type element_type }
  | PRecord { name; fields; range } ->
    let* ctype =
      begin
        match Env.lookup Type name env with
        | Some t -> Ok t
        | None -> type_error range @@ Type_not_in_scope name
      end
    in
    let* record_fields =
      match ctype with
      | CRecord_type { name } -> begin
        match Env.lookup Record name env with
        | Some { fields } -> Ok fields
        | None -> type_error range @@ Record_missing name
      end
      | _ -> type_error range @@ Not_record (box_ctype ctype)
    in
    let field_counter = Hashtbl.create 10 in
    let* cfields =
      let f (name, pvalue) = function
        | Ok acc ->
          let* cvalue = check_expression env pvalue in
          if Option.is_some @@ Hashtbl.find_opt field_counter name then
            type_error (pexpression_range pvalue)
            @@ Initializer_duplicate_field { expected = box_ctype ctype; field = name }
          else begin
            Hashtbl.add field_counter name ();
            match List.assoc_opt name record_fields with
            | None ->
              type_error (pexpression_range pvalue)
              @@ Initializer_no_field { expected = box_ctype ctype; field = name }
            | Some expected ->
              let cexpr_type = cexpression_type cvalue in
              if cexpr_type = expected then
                Ok ((name, cvalue) :: acc)
              else
                type_error (pexpression_range pvalue) @@ type_mismatch expected cexpr_type
          end
        | e -> e
      in
      List.fold_right f fields (Ok [])
    in
    let filter_missing (field_name, _) =
      match Hashtbl.find_opt field_counter field_name with
      | Some _ -> None
      | None -> Some field_name
    in
    begin
      match List.filter_map filter_missing record_fields with
      | field_name :: _ ->
        type_error range
        @@ Initializer_missing_field_value
             { expected = box_ctype ctype; field = field_name }
      | [] -> Ok (CRecord { name; fields = cfields; ctype })
    end
  | PIndex { expression; index; range = _ } ->
    let* cexpression = check_expression env expression in
    let* cindex = check_expression env index in
    begin
      let cexpr_type = cexpression_type cexpression in
      match cexpr_type with
      | CArray_type element_type ->
        Ok (CIndex { expression = cexpression; index = cindex; ctype = element_type })
      | _ -> type_error (pexpression_range expression) @@ Not_array (box_ctype cexpr_type)
    end
  | PAccess { expression; field; range } ->
    let* cexpression = check_expression env expression in
    let cexpr_type = cexpression_type cexpression in
    let* record_fields =
      match cexpr_type with
      | CRecord_type { name } -> begin
        match Env.lookup Record name env with
        | Some { fields } -> Ok fields
        | None -> type_error range @@ Record_missing name
      end
      | _ -> type_error range @@ Not_record (box_ctype cexpr_type)
    in
    begin
      match cexpr_type with
      | CRecord_type { name = _ } -> begin
        let rec find_field field_index = function
          | [] -> type_error range @@ No_field { actual = box_ctype cexpr_type; field }
          | (field_name, field_type) :: rest ->
            if field_name <> field then
              find_field (field_index + 1) rest
            else
              Ok
                (CAccess
                   { expression = cexpression; field; field_index; ctype = field_type })
        in

        find_field 0 record_fields
      end
      | _ ->
        type_error (pexpression_range expression) @@ Not_record (box_ctype cexpr_type)
    end
  | PAssign { target; source; range = _ } ->
    let* ctarget = check_expression env target in
    let* csource = check_expression env source in
    if cexpression_type ctarget = cexpression_type csource then
      Ok (CAssign { target = ctarget; source = csource; ctype = Primitive.unit })
    else
      type_error (pexpression_range source)
      @@ type_mismatch (cexpression_type ctarget) (cexpression_type csource)
  | PApply { func; arguments; range = _ } ->
    let* cfunc = check_expression env func in
    let* cfunc_params, cfunc_result =
      let cfunc_type = cexpression_type cfunc in
      match cfunc_type with
      | CFunction_type { parameters; return_type } -> Ok (parameters, return_type)
      | _ -> type_error (pexpression_range func) @@ Not_function (box_ctype cfunc_type)
    in
    let+ cargs =
      let f expected parg = function
        | Ok acc ->
          let* carg = check_expression env parg in
          let actual = cexpression_type carg in
          if actual = expected then
            Ok (carg :: acc)
          else
            type_error (pexpression_range parg) @@ type_mismatch expected actual
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
          type_error (pexpression_range expression) @@ type_mismatch Primitive.bool ctype
      | Negate ->
        if ctype = Primitive.int || ctype = Primitive.float then
          Ok ctype
        else
          type_error (pexpression_range expression) @@ Not_numeric (box_ctype ctype)
      | Address_of -> Ok (CPointer_type ctype)
      | Dereference -> begin
        match ctype with
        | CPointer_type underlying -> Ok underlying
        | _ -> type_error (pexpression_range expression) @@ Not_pointer (box_ctype ctype)
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
        if cleft_type <> Primitive.bool then
          type_error (pexpression_range right) @@ type_mismatch Primitive.bool cleft_type
        else if cright_type <> Primitive.bool then
          type_error (pexpression_range right) @@ type_mismatch Primitive.bool cright_type
        else
          Ok Primitive.bool
      | Equal | Not_equal ->
        if cleft_type = cright_type then
          Ok cleft_type
        else
          type_error (pexpression_range right) @@ type_mismatch cleft_type cright_type
      | Less
      | Less_equal
      | Greater
      | Greater_equal
      | Add
      | Subtract
      | Multiply
      | Divide
      | Remainder ->
        if cleft_type = Primitive.int || cleft_type = Primitive.float then
          if cleft_type = cright_type then
            Ok cleft_type
          else
            type_error (pexpression_range right) @@ type_mismatch cleft_type cright_type
        else
          type_error (pexpression_range left) @@ Not_numeric (box_ctype cleft_type)
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
      type_error (pexpression_range value)
      @@ type_mismatch cvalue_type_expected cvalue_type_actual
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
        type_error (pexpression_range condition) @@ type_mismatch Primitive.bool ctype
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
      type_error (pexpression_range false_branch)
      @@ If_branch_mismatch
           { true_branch = box_ctype ctrue_type; false_branch = box_ctype cfalse_type }
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
    let body_env = Env.add_all Value cparameters env in
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
          type_error (pexpression_range body) @@ type_mismatch creturn_type cbody_type
    in
    let ctype =
      CFunction_type { parameters = List.map snd cparameters; return_type = creturn_type }
    in
    Ok
      (CClosure
         { parameters = cparameters; return_type = creturn_type; body = cbody; ctype })

and check_block (env : Env.t) : (pblock, cblock) check = function
  | { statements = []; result = None } ->
    Ok { statements = []; result = None; ctype = Primitive.unit }
  | { statements = []; result = Some result } ->
    let+ cexpression = check_expression env result in
    let ctype = cexpression_type cexpression in
    { statements = []; result = Some cexpression; ctype }
  | { statements = PLet { name; value_type; value; range = _ } :: stats; _ } as pblock ->
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
          type_error (pexpression_range value)
          @@ type_mismatch cvalue_type_expected cvalue_type_actual
    in
    let+ cblock =
      check_block (Env.add Value name cvalue_type env) { pblock with statements = stats }
    in
    let clet =
      CLet { name; value_type = cvalue_type; value = cexpression; ctype = Primitive.unit }
    in
    { cblock with statements = clet :: cblock.statements }
  | { statements = stat :: stats; _ } as pblock ->
    let* cexpression = check_expression env stat in
    let+ cblock = check_block env { pblock with statements = stats } in
    { cblock with statements = cexpression :: cblock.statements }

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
    let* cdef, new_env =
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
        let body_env = Env.add_all Value cparameters env in
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
          Ok (cfunction, Env.(add Value) name ctype env)
        else
          type_error (pexpression_range body) @@ type_mismatch creturn_type cbody_type
      | PType_alias { name; value; range = _ } ->
        let+ ctype = check_type env value in
        CType_alias { name; value = ctype }, Env.(add Type) name ctype env
      | PType_record { name; fields; range = _ } ->
        let+ cfields = check_fields fields in
        ( CType_record { name; record = { fields = cfields } }
        , env
          |> Env.(add Type) name (CRecord_type { name })
          |> Env.(add Record) name { fields = cfields } )
    in
    let+ crest = check_definitions new_env pdefs in
    cdef :: crest

let check : (pprogram, cprogram) check = check_definitions Primitive.env

module Typedtree = Typedtree
