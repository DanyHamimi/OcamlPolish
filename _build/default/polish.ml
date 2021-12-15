(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)


open Printf

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block


(***********************************************************************)

let getNextTab(s: string list) : string list = 
  match s with
    | [] -> failwith "tab vide"
    | head::body ->
    begin
      match body with
      | [] -> failwith "pas de suite"
      | _ -> body
    end                
let myread (name:string) : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


let t = "testest"



let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let myprint s =  printf "%s " s

let test (salut: expr) =  match salut with
|Num _ -> printf"VARIABLE SIMPLE";
|Var _ -> printf"JUSTE NOM VAR NON CALCULEE"
|Op _ -> printf"RESULTAT D UN CALCUL"


let makeint val1 = 
  try 
    Some(int_of_string val1) (*Expr : INT*)
  with Failure _ -> None
  
let checkOP (val2:string) = 
  match val2 with
    |"soustraction" -> Some(Sub)
    |"addition" -> Some(Add)
    |"division" -> Some(Div)
    |"multiplication" -> Some(Mul)
    |"modulo" ->  Some(Mod)
    | _ -> None
let getListWord s = String.split_on_char (' ') s (*list de tout les mots de s*)
let printFirstWord s = List.hd(getListWord(s))

let rec returnfirstOP (s:string list)(s1 : string) :string = match s with
  | [] -> s1
  | head::body ->
    begin
      match head with
        |"" -> "vide"
        |"-" -> "soustraction"
        |"+" -> "addition"
        |"/" -> "division"
        |"*" -> "multiplication"
        |"%" -> "modulo"
        | _ -> returnfirstOP (body) (head)
    end

let cmd s = match s with
  | "COMMENT" -> print_endline "sup"
  | "READ" -> print_endline "lecture" 
  | "IF" -> print_endline "si"
  | "ELSE" -> print_endline "sinon"
  | "WHILE" -> print_endline "Pendant"
  | " " -> print_endline "1 espace"
  | "\t" -> print_endline "2 espace"
  | "" -> print_string "rien"
  | _ -> print_string "rien"
  
let rec function2 s = match s with
  | [] -> print_endline "-ligne vide-"
  | head::body ->
    begin
      match head with
        |"" -> printf"vide"
        |"-" -> printf"soustraction"
        |"+" -> printf"addition"
        |"/" -> printf"division"
        | _ ->  myprint (head);
                function2 body;
    end
let rec getOpes (s: string list) : string list = 
  match s with
    | [] -> failwith "errortoto"
    | head::body ->
      begin
        match head with
        | ":=" -> getNextTab(body)
        | _ -> getOpes body
      end

let retBod (s: string list): string list =
  match s with
    |[]->[]
    |head::body->body

(*let rec makeExpr (task : string list) :(expr * string list)=
  match task with 
  | [] -> failwith "vide"
  | _ -> match makeint (returnfirstOP task (List.hd(task))) with
    | Some n -> ((Num n),(retBod task))(*Dans le cas ou l'expression makeint return une val directement on prend le case Num*)
    | None ->
      match checkOP (returnfirstOP task (List.hd(task))) with
      | Some ope -> let(e1,reste1)=makeExpr (getOpes task) in let(e2,reste2)=makeExpr (getNextTab(getOpes task)) in 
        (Op(ope,e1,e2),(retBod task))
      | None -> (Var(returnfirstOP task (List.hd(task))),(retBod task))*)

let rec makeExpr (task: string list) : (expr * string list) =
  match task with
  | [] -> failwith "vide"
  | h :: b ->
     match makeint h with
     | Some n -> (Num n, b)
     | None ->
        match checkOP (returnfirstOP task h) with
        | Some op -> let (e1,tks1) = makeExpr b in
                 let (e2,tks2) = makeExpr tks1 in
                 (Op (op, e1, e2), tks2)
        | None -> (Var h, b)      

let rec getFristCmd l j= match l with
  | [] -> print_endline "-ligne vide-"
  | head::body -> 
    begin
      match head with
        |"" -> getFristCmd body (j+1)
        |"COMMENT" -> printf"non"
        |"READ" -> printf"%s" (List.hd(body))
        |"IF" -> printf "if...%i" j
        |"ELSE" -> printf"else.."
        |"PRINT" -> printf"%s...%i" (List.hd(body)) (j)
        | _ -> printf "s"
    end
    ;;


(*let retRead List = Rea*)


(*let ifStatement (fileString:string list) (pos:int)*)
(*Si après l'espace c'est direct un int on fait ça*)

(*Si après l'espace c'est une opé on fait ça*)

let setInstr(inst:string list) : instr =
  match inst with
    |[] -> failwith "instr vide"
    |head::body -> let (expr,reste)= makeExpr(body) in Set (head,expr)

let returnComp(s:string) : comp = 
  match s with
   |"="-> Eq
   |"<>"-> Ne
   |"<"-> Lt
   |"<="-> Le
   |">"-> Gt
   |">="-> Ge
   |_->failwith "erreur pas de comp"


let returnCond (condL:string list) : cond  =
  match condL with
   |[] -> failwith "cond vide" 
   |_ -> let (ex1, reste) = makeExpr(condL) in 
    match reste with
      |[]->failwith("pas de cond")
      |head::body-> let (ex2,reste)=makeExpr(body) in (ex1,(returnComp head),ex2)




let rec moveIndent (line:string list)(ind:int): string list=
  if ind<>0 then
    match line with 
      |[]->failwith"err"
      |head::body->moveIndent body (ind-1)
  else    
    line



let moveBod (line:string list)(mov:int): string list=
if mov<>0 then
    match line with 
      |[]->failwith"err"
      |head::body->moveIndent body (mov-1)
  else    
    line

let iMov = 0

(*let returnMoveBody (s:string list) (pos:int) (indent:int) : program = let iMov=iMov+1 in returnPrgm s (pos) (indent)*)
 
let rec returnPrgm(fileString:string list) (pos:int) (indent:int) : program =
  match fileString with
  |[]->[]
  |head::body->
    match moveIndent(getListWord(head)) (indent) with
      |[]->  failwith("marche pas")
      |h::t->
        match h with
          | "COMMENT" -> returnPrgm(body) (pos+1) indent
          | "READ" ->  [pos,Read (List.hd t)]@returnPrgm(body) (pos+1) indent 
          | "PRINT" -> let (expr, reste)=makeExpr(t) in [pos,Print (expr)]@returnPrgm(body) (pos+1) indent 
          (*| "IF" -> let cnd = returnCond(t) in 
            let (block1) = returnPrgm (body (pos+1) (indent+2) ) in *)
          | "WHILE" -> let cnd = returnCond(t) in
            [pos,While (cnd,returnPrgm body (pos+1) (indent+2) )]@returnPrgm(body) (pos+1) indent 
          |""->returnPrgm(body) (pos+1) indent 
          | _ -> [pos, setInstr(t)]@returnPrgm (body) (pos+1) indent 
      
  

       


let printList (s) = List.iter (myprint) (getListWord(s))
let rec print_list_string myList i= match myList with
  | [] -> print_endline "Fin de fichier"
  | head::body -> 
    begin
    printf "%i: " i;
    getFristCmd (getListWord(head)) (0);
    printf "\n";
    print_list_string (body) (i+1)
    end
    ;;


let read_polish (filename:string) : program = returnPrgm (myread(filename)) 1 0 

let print_polish (p:program) : unit = failwith "TODO"

let eval_polish (p:program) : unit = failwith "TODO"
let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish(read_polish(file))
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | _ -> usage()

(* lancement de ce main *)
let () = main ()
