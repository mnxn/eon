module Map = Map.Make (String)

type t =
  { types : Typedtree.ctype Map.t
  ; values : Typedtree.ctype Map.t
  }

let empty = { types = Map.empty; values = Map.empty }

let add_value name value env = { env with values = Map.add name value env.values }

let add_values bindings env =
  let bindings = List.to_seq bindings in
  { env with values = Map.add_seq bindings env.values }

let add_type name value env = { env with types = Map.add name value env.types }

let add_types bindings env =
  let bindings = List.to_seq bindings in
  { env with types = Map.add_seq bindings env.types }

let lookup_value name env = Map.find_opt name env.values

let lookup_type name env = Map.find_opt name env.types
