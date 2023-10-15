module Map = Map.Make (String)

type t =
  { types : Typedtree.ctype Map.t
  ; values : Typedtree.ctype Map.t
  ; records : Typedtree.crecord Map.t
  }

type 'a binding_type =
  | Type : Typedtree.ctype binding_type
  | Value : Typedtree.ctype binding_type
  | Record : Typedtree.crecord binding_type

let empty = { types = Map.empty; values = Map.empty; records = Map.empty }

let add (type a) (binding_type : a binding_type) name (value : a) env =
  match binding_type with
  | Type -> { env with types = Map.add name value env.types }
  | Value -> { env with values = Map.add name value env.values }
  | Record -> { env with records = Map.add name value env.records }

let add_all (type a) (binding_type : a binding_type) (bindings : (string * a) list) env =
  let bindings = List.to_seq bindings in
  match binding_type with
  | Type -> { env with types = Map.add_seq bindings env.types }
  | Value -> { env with values = Map.add_seq bindings env.values }
  | Record -> { env with records = Map.add_seq bindings env.records }

let lookup (type a) (binding_type : a binding_type) name env : a option =
  match binding_type with
  | Type -> Map.find_opt name env.types
  | Value -> Map.find_opt name env.values
  | Record -> Map.find_opt name env.records
