let () =
  if Array.length Sys.argv < 2 then print_endline "usage: catml <file>"
  else CatML.Main.run_file Sys.argv.(1)
