let () =
  if Array.length Sys.argv < 2 then
    print_endline "no source file specified"
  else
    let filename = Sys.argv.(1) in
    Eon_driver.run filename
