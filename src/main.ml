open Lang

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let parse_file (path : string) : expr option =
  try
    let s = In_channel.with_open_text path In_channel.input_all in
    Some (parse s)
  with
  | Parser.Error ->
      print_endline "Syntax_error";
      None
  | Failure msg ->
      print_endline ("Lexing error: " ^ msg);
      None
  | Sys_error msg ->
      print_endline ("File error: " ^ msg);
      None

let run_file (path : string) : unit =
  match parse_file path with
  | None -> ()
  | Some e ->
      begin match type_of_program e with
      | None -> print_endline "Type Error"
      | Some t ->
          let r = multistep e in
          if is_val r then
            print_endline (string_of_expr r ^ " : " ^ string_of_type t)
          else print_endline ("Stuck: " ^ string_of_expr r)
      end
