let _ = 
  let input_filename = Sys.argv.(1) in
  let in_chan = open_in input_filename in
  let verbose = ref false in
  let options = [
    "-v", Arg.Set verbose, "Verbose mode";
  ] in
    Arg.parse options (fun x -> ()) "Options: ";
    let instructions = ref [] in
      try
	while true do
          print_endline "read";
	  instructions := (Machine.input_instruction in_chan) :: !instructions
	done
      with End_of_file ->
	instructions := List.rev !instructions ;
	if !verbose then 
	  print_endline (Machine.string_of_instructions !instructions); 
	let result = Machine.eval (!instructions, [], []) in
	  print_endline (Machine.string_of_value result)
	    
