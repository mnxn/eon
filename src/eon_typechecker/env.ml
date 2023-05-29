module Map = Map.Make (String)

type t =
  { types : Typedtree.ctype Map.t
  ; values : Typedtree.ctype Map.t
  }

type binding_type =
  | Type
  | Value

let empty = { types = Map.empty; values = Map.empty }

let add binding_type name value env =
  match binding_type with
  | Type -> { env with types = Map.add name value env.types }
  | Value -> { env with values = Map.add name value env.values }

let add_all binding_type bindings env =
  let bindings = List.to_seq bindings in
  match binding_type with
  | Type -> { env with types = Map.add_seq bindings env.types }
  | Value -> { env with values = Map.add_seq bindings env.values }

let lookup binding_type name env =
  match binding_type with
  | Type -> Map.find_opt name env.types
  | Value -> Map.find_opt name env.values
