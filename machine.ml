type expression = 
  | Integer of int
  | Var of string
  | Add of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Lambda of string * expression
  | Let of string * expression * expression
  | LetRec of string * expression * expression
  | App of expression * expression

and instruction = 
  | IConst of int
  | IAdd
  | ISub
  | IMul
  | IAccess of int
  | IClosure of code
  | ILet
  | IEndLet
  | IApply
  | ITailApply
  | IReturn
  | IVarFree of int

and value = 
  | VVarFree of int
  | VInt of int
  | VClosure of code * env
  | VCode of code
  | VEnv of env

and env = value list
and code = instruction list

(* and machine = int list * instr list *)

type dvar = 
  | Free of int
  | Bounded of int

type dterm = 
  | DInt of int
  | DVar of dvar
  | DAbs of dterm
  | DApp of dterm * dterm
  | DLet of dterm * dterm
  | DAdd of dterm * dterm
  | DSub of dterm * dterm
  | DMul of dterm * dterm

module Env = Map.Make(struct type t = string let compare = String.compare end)

let dterm_of_term envb term =
  let envf = ref Env.empty in
  let rec dterm_of_term envb = function
  | Integer n -> DInt n
  | Add (e1, e2) -> DAdd (dterm_of_term envb e1, dterm_of_term envb e2)
  | Sub (e1, e2) -> DSub (dterm_of_term envb e1, dterm_of_term envb e2)
  | Mul (e1, e2) -> DMul (dterm_of_term envb e1, dterm_of_term envb e2)
  | Var s ->
    if Env.mem s envb then
      (DVar (Bounded (Env.find s envb)))
    else
      if Env.mem s !envf then
	(DVar (Free (Env.find s !envf)))
      else
	begin
	  let n = Env.cardinal !envf in
	  envf := Env.add s n !envf ;
	  (DVar (Free n))
	  end 
  | Lambda (s, t) -> 
    DAbs (dterm_of_term (Env.add s 0 (Env.map succ envb)) t)
  | App (t1, t2) -> 
    DApp (dterm_of_term envb t1, dterm_of_term envb t2)
  | Let (v, t1, l) ->
    DLet (dterm_of_term envb t1,
          dterm_of_term (Env.add v 0 (Env.map succ envb)) l)
  | LetRec (v, t1, l) ->
    DLet (dterm_of_term (Env.add v 0 (Env.map succ envb)) t1,
          dterm_of_term (Env.add v 0 (Env.map succ envb)) l)
  in
    dterm_of_term envb term

let rec tcompile = function
  | DApp (e1, e2) -> (compile e1) @ (compile e2) @ [ITailApply]
  | _ as a -> (compile a) @ [IReturn]

and compile = function
  | DInt n -> [IConst n]
  | DVar (Bounded n) -> [IAccess n]
  | DVar (Free n) -> [IVarFree n]
  | DAbs e -> [IClosure ((tcompile e))]
  | DApp (e1, e2) -> (compile e1) @ (compile e2) @ [IApply]
  | DLet (e1, e2) -> (compile e1) @ [ILet] @ (compile e2) @  [IEndLet]
  | DAdd (e1, e2) -> (compile e1) @ (compile e2) @ [IAdd]
  | DSub (e1, e2) ->  (compile e1) @ (compile e2) @ [ISub]
  | DMul (e1, e2) ->  (compile e1) @ (compile e2) @ [IMul]

(*  | Let (s, e1, e2) -> (compile e1) @ [ILet] @ (compile e2) @ [IEndLet] *)

let rec eval (instructions, env, stack) = 
  match instructions with
    | [] -> print_endline "(Empty)" ;
      List.hd stack
    | IConst n :: rest -> print_endline "IConst" ;
      eval (rest, env, VInt(n) :: stack)
    | IAdd :: rest -> print_endline "IAdd" ;
	begin
	  match stack with
	    | VInt(n2) :: VInt(n1) :: stack -> 
		eval (rest, env, VInt(n1 + n2) :: stack)
	    | _ -> failwith "Add needs two arguments"
	end
    | ISub :: rest -> print_endline "ISub" ;
	begin
	  match stack with
	    | VInt(n2) :: VInt(n1) :: stack -> 
		eval (rest, env, VInt(n1 - n2) :: stack)
	    | _ -> failwith "Sub needs two arguments"
	end
    | IMul :: rest -> print_endline "IMul" ;
	begin
	  match stack with
	    | VInt(n2) :: VInt(n1) :: stack -> 
		eval (rest, env, VInt(n1 * n2) :: stack)
	    | _ -> failwith "Sub needs two arguments"
	end
    | IAccess n :: rest -> print_endline "IAccess" ;
	eval (rest, env, (List.nth env n) :: stack)
    | ILet :: rest -> print_endline "ILet" ;
	eval (rest, (List.hd stack) :: env, List.tl stack)
    | IEndLet :: rest -> print_endline "IEndLet" ;
	eval (rest, List.tl env, stack)
    | IClosure c' :: c -> print_endline "IClosure" ;
	eval (c, env, VClosure (c', env) :: stack)
    | IApply :: c -> print_endline "IApply" ;
	begin
	  match stack with
	      v :: VClosure (c', env') :: stack ->
		eval (c', v :: env', stack)
	    | _ -> failwith "IApply"
	end
    | ITailApply :: c -> print_endline "ITailApply" ;
      begin
        match stack with
	  v :: VClosure (c', env') :: stack ->
		eval (c', v :: env', VCode(c) :: VEnv(env) :: stack)
	| _ -> failwith "IApply"
      end
    | IReturn :: _ -> print_endline "IReturn" ;
	begin
	  match stack with
	    | v :: VCode(c') :: VEnv(e') :: s ->
		eval (c', e', v :: s)
	    | _ -> failwith "IReturn"
	end
    | IVarFree n :: rest -> print_endline "IVarFree" ;
	eval (rest, env, VVarFree n :: stack)

let rec output_instruction out_chan = function
  | IConst n -> output_byte out_chan 0 ; output_binary_int out_chan n
  | IAdd -> output_byte out_chan 1
  | ISub -> output_byte out_chan 2
  | IMul -> output_byte out_chan 21
  | IAccess n -> output_byte out_chan 3 ; output_binary_int out_chan n
  | IClosure code -> 
      output_byte out_chan 4 ; List.iter (output_instruction out_chan) code
  | ILet -> output_byte out_chan 5
  | IEndLet -> output_byte out_chan 6
  | IApply -> output_byte out_chan 7
  | ITailApply -> output_byte out_chan 71
  | IReturn -> output_byte out_chan 8 
  | IVarFree n -> output_byte out_chan 9 ; output_binary_int out_chan n

let input_closure in_chan =
  let rec input_closure accu = 
    match input_byte in_chan with
      | 0 -> let n = input_binary_int in_chan in
	  input_closure (IConst n :: accu)
      | 1 -> input_closure (IAdd :: accu)
      | 2 -> input_closure (ISub :: accu)
      | 21 -> input_closure (IMul :: accu)
      | 3 -> let n = input_binary_int in_chan in
	  input_closure (IAccess n :: accu)
      | 4 -> let code = input_closure [] in
	  input_closure (IClosure code :: accu)
      | 5 -> input_closure (ILet :: accu)
      | 6 -> input_closure (IEndLet :: accu)
      | 7 -> input_closure (IApply :: accu)
      | 71 -> input_closure (ITailApply :: accu)
      | 8 -> List.rev (IReturn :: accu)
      | 9 -> let n = input_binary_int in_chan in
	  input_closure (IVarFree n :: accu)
      | _ -> failwith "Unkown opcode"
  in
    input_closure []

let input_instruction in_chan = 
  match input_byte in_chan with
    | 0 -> IConst (input_binary_int in_chan)
    | 1 -> IAdd
    | 2 -> ISub
    | 21 -> IMul
    | 3 -> IAccess (input_binary_int in_chan)
    | 4 -> IClosure (input_closure in_chan)
    | 5 -> ILet
    | 6 -> IEndLet
    | 7 -> IApply
    | 71 -> ITailApply
    | 8 -> IReturn
    | 9 -> IVarFree (input_binary_int in_chan)
    | _ -> failwith "Unkown opcode"

let rec string_of_expression = function
  | Integer n -> string_of_int n
  | Var s -> s
  | Add (e1, e2) -> "(" ^ (string_of_expression e1) ^ " + " ^ 
      (string_of_expression e2) ^ ")"
  | Sub (e1, e2) -> "(" ^ (string_of_expression e1) ^ " - " ^ 
    (string_of_expression e2) ^ ")"
  | Mul (e1, e2) -> "(" ^ (string_of_expression e1) ^ " - " ^ 
    (string_of_expression e2) ^ ")"
  | Lambda (s, e) -> "fun " ^ s ^ " -> " ^ (string_of_expression e)
  | Let (s, e1, e2) -> "let " ^ s ^ " = " ^ (string_of_expression e1) ^ 
    " in " ^ (string_of_expression e2)
  | LetRec (s, e1, e2) -> "letrec " ^ s ^ " = " ^ (string_of_expression e1) ^ 
    " in " ^ (string_of_expression e2)
  | App (e1, e2) -> (string_of_expression e1) ^ " " ^ (string_of_expression e2)

let rec string_of_instruction = function
  | IConst n -> "CONST " ^ (string_of_int n)
  | IAdd -> "ADD"
  | ISub -> "SUB"
  | IMul -> "MUL"
  | IAccess n -> "ACCESS " ^ (string_of_int n)
  | IClosure is -> "CLOSURE " ^ (string_of_instructions is)
  | ILet -> "LET"
  | IEndLet -> "ENDLET"
  | IApply -> "APPLY"
  | ITailApply -> "TAILAPPLY"
  | IReturn -> "RETURN"
  | IVarFree n -> "VARFREE " ^ (string_of_int n)

and string_of_instructions = function
  | [] -> ""
  | instruction :: [] ->
      (string_of_instruction instruction)
  | instruction :: rest -> 
      (string_of_instruction instruction) ^ " " ^ (string_of_instructions rest)

let rec string_of_value = function
  | VVarFree n -> string_of_int n
  | VInt n -> string_of_int n
  | VClosure (code, env) -> 
      "CLOSURE " ^ (string_of_instructions code)
  | VCode code -> string_of_instructions code
  | VEnv env -> string_of_values env

and string_of_values = function
  | [] -> ""
  | value :: rest ->
      (string_of_value value) ^ " " ^ (string_of_values rest)

let rec string_of_dterm = function
  | DInt n -> (string_of_int n)
  | DAdd (e1, e2) -> 
    " (" ^ (string_of_dterm e1) ^ " + " ^ (string_of_dterm e2) ^ ")"
  | DSub (e1, e2) -> 
    " (" ^ (string_of_dterm e1) ^ " - m" ^ (string_of_dterm e2) ^ ")"
  | DMul (e1, e2) -> 
    " (" ^ (string_of_dterm e1) ^ " * m" ^ (string_of_dterm e2) ^ ")"
  | DVar (Free n) -> "F(" ^ (string_of_int n) ^ ")"
  | DVar (Bounded n) -> "B(" ^ (string_of_int n) ^ ")"
  | DAbs (t) -> " (fun -> " ^ (string_of_dterm t) ^ ")"
  | DApp (t1, t2) -> 
      " (" ^ (string_of_dterm t1) ^ " " ^ (string_of_dterm t2) ^ ")"
  | DLet (t1, t2) ->
    "let " ^ (string_of_dterm t1) ^ " in " ^ (string_of_dterm t2)
