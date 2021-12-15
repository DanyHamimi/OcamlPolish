(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)


open Printf
open Type

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
(***********************************************************************)
(*renvoie tout un tab sans le premiere élément*)
let getNextTab(s: string list) : string list = 
  match s with
    | [] -> failwith "tab vide"
    | head::body ->
    begin
      match body with
      | [] -> failwith "pas de suite"
      | _ -> body
    end  
            
    
(*Ouvre le fichier et return une string list, de tout les lignes*)
let myread (name:string) : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let myprint s =  printf "%s " s


(*String -> int*)
let makeint val1 = 
  try 
    Some(int_of_string val1) (*Expr : INT*)
  with Failure _ -> None
  
(*Transforme un string en Op*)
let checkOP (val2:string) = 
  match val2 with
    |"-" -> Some(Sub)
    |"+" -> Some(Add)
    |"/" -> Some(Div)
    |"*" -> Some(Mul)
    |"%" -> Some(Mod)
    | _ -> None
         
let getListWord s = String.split_on_char (' ') s (*list de tout les mots d'un String*)
let printFirstWord s = List.hd(getListWord(s))



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


let rec makeExpr (task: string list) : (expr * string list) =
  match task with
  | []     -> failwith "vide"
  | h :: b ->
     match makeint h with
     | Some n -> (Num n, b)
     | None   ->
        match checkOP h with
        | Some op ->
           let (e1, tks1) = makeExpr b in
           let (e2, tks2) = makeExpr tks1 in
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

let setInstr(inst:string list) : instr =
  match inst with
    |[] -> failwith "instr vide"
    |head::body -> let (expr,reste)= makeExpr(body) in Set (head,expr)

let returnComp(s:string) : comp = 
  match s with
   |"="  -> Eq
   |"<>" -> Ne
   |"<"  -> Lt
   |"<=" -> Le
   |">"  -> Gt
   |">=" -> Ge
   |_    ->failwith "erreur pas de comp"

(*se déplace de 'ind' espace, ind correspondant  *)
let rec moveIndent (line:string list)(ind:int): string list=
  if ind <> 0 then
    match line with 
      | [] -> failwith"err"
      |head::body->moveIndent body (ind-1)
  else    
    line

(*Donne l'indentation d'une ligne*)
let rec getIndent (words:string list) : int =
  match words with
  | h :: t -> if h = "" then 1 + getIndent t else 0
  | []     -> 0


(*string -> condition*)
 
let returnCond (condL:string list) : cond  =
  match condL with
   | [] -> failwith "cond vide" 
   | _  -> let (ex1, reste) = makeExpr condL in 
    match reste with
      | [] -> failwith("pas de cond")
      | head :: body ->
         let (ex2,reste) = makeExpr(body) in
         (ex1, returnComp head, ex2)


(* liste de ligne(fichier)-> program*)
let rec returnPrgm (fileString:string list) (pos:int) (indent:int) : (program * string list) =
  match fileString with
  | []         -> ([], [])
  | head::body ->
     if getIndent (getListWord head) <> indent (*vérifie si l'indent est diff de getIndent*)
     then ([], fileString)
     else
       let ii = getIndent (getListWord head)
       in ()(***);
      match moveIndent (getListWord head) indent with
      | []   -> failwith("marche pas")
      | h::t ->
        match h with
          | "COMMENT" -> returnPrgm body (pos+1) indent (*passe à la suite si COMMENT*)
          | "READ"    -> 
             let (prg, reste) = returnPrgm body (pos+1) indent in
             ((pos, Read (List.hd t)) :: prg, reste)
          | "PRINT"   ->
             let (expr, resteExpr) = makeExpr t in 
             let (prg, reste) = returnPrgm body (pos+1) indent in
             ((pos,Print expr) :: prg, reste)
          | "WHILE" ->
             let cnd = returnCond(t) in
             let (block1, reste)  = returnPrgm body  (pos+1) (indent+2) in
             let (prg,    reste2) = returnPrgm reste (pos+1) indent in 
             ((pos, While (cnd, block1)) :: prg, reste2)   
          | "IF" ->
             let cnd = returnCond(t) in 
             let (block1, reste) = returnPrgm body (pos+1) (indent+2) in
             (match reste with
             | rh :: rt ->
                    (match moveIndent (getListWord rh) indent with
                    | [ "ELSE" ] ->
                       let (block2, reste2) = returnPrgm rt     (pos+1) (indent+2) in
                       let (prg, reste3)    = returnPrgm reste2 (pos+1) indent     in
                       ((pos, If (cnd, block1, block2)) :: prg, reste3)
                    | _ -> 
                       let (prg, reste2) = returnPrgm reste (pos+1) indent in 
                       ((pos, If (cnd, block1, [])) :: prg, reste2))
             | [] -> ([],[]))
          | _ ->
             let (prg, reste) = returnPrgm body (pos+1) indent in 
             ((pos, setInstr(t)) :: prg, reste)


      
(*fonction de test*)
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


    let read_polish (filename:string) : program =
      let (prg, reste) = returnPrgm (myread(filename)) 1 0
      in match reste with
         | [] -> prg
         | _ -> failwith "parsing incomplet"   
let rec printex exp = 
  match exp with 
  |Num(n) -> myprint(string_of_int n)
  |Var(s) -> myprint(s)
  |Op(op1,ex1,ex2)-> print_expB op1 ex1 ex2;
  and
  print_expB op1 ex1 ex2=
    match op1 with
      | Add -> myprint("+ "); 
               printex ex1;
               myprint(" ");
               printex ex2
      | Sub -> myprint("- ");
               printex ex1;
               myprint(" ");
               printex ex2
      | Mul -> myprint("* ");
               printex ex1;
               myprint(" ");
               printex ex2
      | Div -> myprint("/ ");
               printex ex1;
               myprint(" ");
               printex ex2
      | Mod -> myprint("% ");
               printex ex1;
               myprint(" ");
               printex ex2

let printOP comparaison ex1 ex2 = 
  match comparaison with
  | Eq -> printex ex1;
          myprint(" = ");
          printex ex2
  | Ne -> printex ex1;
          myprint(" <> ");
          printex ex2
  | Lt -> printex ex1;
          myprint(" < ");
          printex ex2
  | Gt -> printex ex1;
          myprint(" > ");
          printex ex2
  | Ge -> printex ex1;
          myprint(" >= ");
          printex ex2
  | Le -> printex ex1;
          myprint(" <= ");
          printex ex2
  let print_cond c :unit=
    let (ex1,cmp1,ex2) = c in 
      printOP cmp1 ex1 ex2
  ;; 
  let rec print_polishBlock (blocka:block)=
    match blocka with
    | [] -> ();
    | (v,i)::next -> match i with
              |Set(n,e) -> myprint(n);
                           myprint(" :=");
                           printex e;
                           print_newline();
                           print_polishBlock (next);
              | Read(n) -> myprint("READ");myprint(n);
                           print_newline();
                           print_polishBlock (next);
              | Print(e) -> myprint("PRINT ");
                            printex e;
                            print_newline();
                            print_polishBlock (next);
              | If(c,b1,b2) -> myprint("IF");
                               print_cond c;
                               print_newline();
                               print_polishBlock b1;
                               myprint("ELSE");
                               print_newline();
                               print_polishBlock b2;
                               print_polishBlock (next);
              | While(c,b) -> myprint("WHILE");
                              print_cond c;
                              print_newline();
                              print_polishBlock b;
                              print_newline();
                              print_polishBlock (next);;
let print_polish (p:program) : unit = print_polishBlock p

let eval_polish (p:program) : unit = failwith "TODO"
let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish(read_polish(file))
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | _ -> usage()

(* lancement de ce main *)
let () = main ()