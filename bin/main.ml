let () =
  let args = List.tl (Array.to_list Sys.argv) in
  Eon_driver.run args
