let _ = 
  let input_filename = Sys.argv.(1) in
  let output_filename = (Filename.chop_suffix input_filename ".source") ^ 
    ".bytecode" in
  let verbose = ref false in
  let options = [
    "-v", Arg.Set verbose, "Verbose mode";
  ] in
    Arg.parse options (fun x -> ()) "Options: ";
  let in_chan = open_in input_filename in
  let out_chan = open_out output_filename in
  let lexbuf = Lexing.from_channel in_chan in
  let expression = Parser.start Lexer.token lexbuf in
  let dterm = Machine.dterm_of_term Machine.Env.empty expression in
  let instructions = Machine.compile dterm in
  print_int (List.length instructions) ; print_newline () ;
    if !verbose then 
      print_endline (Machine.string_of_dterm dterm) ;
      print_endline (Machine.string_of_instructions instructions) ;
    List.iter 
      (fun i -> print_endline "i" ;
        (Machine.output_instruction out_chan i)) instructions ;
    close_out out_chan
