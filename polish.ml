(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)


open Printf
open Type
open Func

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
(***********************************************************************)
(*renvoie tout un tab sans le premiere élément*)


  
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
  
(*Récupère les opérations*)
let rec getOpes (s: string list) : string list = 
  match s with
    | [] -> failwith "errortoto"
    | head::body ->
      begin
        match head with
        | ":=" -> getNextTab(body)
        | _ -> getOpes body
      end



(*transforme une list de string en expr avec la suite*)
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
(*récupère la premiere commande d'une ligne*)
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
(*Définit une nouvelle instruction*)
let setInstr(inst:string list)(nameV:string) : instr =
  match inst with
    |[] -> failwith "instr vide"
    |head::body -> let (expr,reste)= makeExpr(body) in Set (nameV,expr)
(*Retourne le comparateur sous la forme d'un string*)
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
 (*trouve la condition et la retourne*)
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
             ((pos, setInstr(t) (h)) :: prg, reste)


      
(*Transformer une list en string*)
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

(*transforme un fichier donné en argument en program*)
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
(*affiche les opération d'une comparaison*)
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
(*affiche l'indentation avant la ligne de code dans print*)
let rec print_ind (ind:int)  =  
  match ind with
    |0->myprint("")
    |_->myprint("  ");
        print_ind(ind-1)
  

(*Affiche un block sous la forme de code*)
  let rec print_polishBlock (blocka:block) (ind:int)=
    match blocka with
    | [] -> ();
    | (v,i)::next -> print_ind(ind);
    match i with
              |Set(n,e) -> myprint(n);
                           myprint(" :=");
                           printex e;
                           print_newline();
                           print_polishBlock (next)(ind);
              | Read(n) -> myprint("READ");myprint(n);
                           print_newline();
                           print_polishBlock (next)(ind);
              | Print(e) -> myprint("PRINT ");
                            printex e;
                            print_newline();
                            print_polishBlock (next)(ind);
              | If(c,b1,b2) -> myprint("IF");
                               print_cond c;
                               print_newline(); 
                               print_polishBlock b1 (ind+1);
                               myprint("ELSE");
                               print_newline();
                               print_polishBlock b2 (ind+1);
                               print_polishBlock (next)(ind);
              | While(c,b) -> myprint("WHILE");
                              print_cond c;
                              print_newline();
                              print_polishBlock b (ind+1);
                              print_polishBlock (next)(ind);;
(*Table contenant toutes les variables et leurs valeurs*)
let my_hash = Hashtbl.create 0;;
(*Table contenant toutes les variables*)
let map_allVals = Hashtbl.create 0;;
(*Table contenant toutes les variables initialisées au moins une fois*)
let map_errVals = Hashtbl.create 0;;
(*récupère la valeur d'une expression donnée en argument*)
let rec getVal1 (valExpr:expr) : int = 
  match valExpr with 
  |Num(i) -> Hashtbl.replace my_hash (string_of_int i) i;
             i
  |Var(n) -> Hashtbl.find my_hash n;
  |Op(op,exp1,exp2) -> match op with
                        | Add ->  getVal1 exp1 + getVal1 exp2;
                        | Sub ->  getVal1 exp1 - getVal1 exp2;
                        | Mul ->  getVal1 exp1 * getVal1 exp2;
                        | Div ->  getVal1 exp1 / getVal1 exp2;
                        | Mod ->  (getVal1 exp1) mod (getVal1 exp2)
                      
(*trouver dans le tableau la valeur d'une exp*)   
let findInTableVal (valExp:expr) : string =
  match valExp with 
  (*|Num(n) -> (string_of_int n)*)
    |Var(s) -> (s);
    |Num(i) -> Hashtbl.replace my_hash (string_of_int i) i;
              string_of_int(i)
    |Op(op,exp1,exp2) ->  match op with
                            | Add ->  string_of_int(getVal1 exp1 + getVal1 exp2);
                            | Sub ->  string_of_int(getVal1 exp1 - getVal1 exp2);
                            | Mul ->  string_of_int(getVal1 exp1 * getVal1 exp2);
                            | Div ->  string_of_int(getVal1 exp1 / getVal1 exp2);
                            | Mod ->  string_of_int((getVal1 exp1) mod (getVal1 exp2))


(*Récupérer la valeur d'une expression*)
let rec getValExprs (valExpr:expr) = 
  match valExpr with
    | Num(a) -> a
    | Var(a) ->  Hashtbl.find my_hash a
    |Op(op1,ex1,ex2)-> calculB op1 ex1 ex2;
    and
    calculB op1 ex1 ex2=
      match op1 with
        | Add ->  getVal1 ex1 + getVal1 ex2;
        | Sub ->  getVal1 ex1 - getVal1 ex2;
        | Mul ->  getVal1 ex1 * getVal1 ex2;
        | Div ->  getVal1 ex1 / getVal1 ex2;
        | Mod ->  (getVal1 ex1) mod (getVal1 ex2)


(*vérifie si la condition donnée en argument est valide ou non -> bool*)
let checkCond(c:cond)=
      let (b1,comparasion,b2) = c in
      match comparasion with
      | Eq -> if(Hashtbl.find my_hash (findInTableVal b1) == Hashtbl.find my_hash (findInTableVal b2)) then
                true
              else
                false
      | Ne -> if(Hashtbl.find my_hash (findInTableVal b1) != Hashtbl.find my_hash (findInTableVal b2)) then
                true
              else
                false
      | Lt -> if(Hashtbl.find my_hash (findInTableVal b1) < Hashtbl.find my_hash (findInTableVal b2)) then
                true
              else
                false
      | Gt -> if(Hashtbl.find my_hash (findInTableVal b1) > Hashtbl.find my_hash (findInTableVal b2)) then
                true
              else
                false
      | Ge -> if(Hashtbl.find my_hash (findInTableVal b1) >= Hashtbl.find my_hash (findInTableVal b2)) then
                true
              else
                false
      | Le -> if(Hashtbl.find my_hash (findInTableVal b1) <= Hashtbl.find my_hash (findInTableVal b2)) then
                true
              else
                false
  let check_Cond c =
    let (ex1,cmp1,ex2) = c in 
      checkCond(cmp1 ex1 ex2)
  ;; 




let setVal (expression:expr) = 
  match expression with
   | Num(i) -> 1;
   | _ -> 1
(*affiche le résultat du calcul d'une expression*)
let printExpCalc (expression:expr) = 
  match expression with 
  | Num(a) -> a
  | Var(a) ->  Hashtbl.find my_hash a
  | Op(op,exp1,exp2) ->  match op with
                      | Add ->  getVal1 exp1 + getVal1 exp2;
                      | Sub ->  getVal1 exp1 - getVal1 exp2;
                      | Mul ->  getVal1 exp1 * getVal1 exp2;
                      | Div ->  getVal1 exp1 / getVal1 exp2;
                      | Mod ->  (getVal1 exp1) mod (getVal1 exp2)
(*Prend un block en argument et affiche les résultats après "compilation"*)
let rec evalPol(blocka:block): unit =
  match blocka with
  | [] -> ();
  | (v,i)::next -> match i with
          |Set(n,e) ->
                      Hashtbl.replace my_hash n (getValExprs(e));
                      evalPol (next);
          | Read(n) ->
                      printf"Please enter a value for %s : " n;
                      let nVal = read_int() in ();
                      Hashtbl.replace my_hash n nVal;
                      evalPol (next);
          | Print(e) -> printex e;
                        myprint(" = ");
                        myprint(string_of_int(printExpCalc(e)));
                        print_newline();
                        evalPol (next);
          | If(c,b1,b2) ->
                          if(checkCond c) then
                              evalPol b1
                          else
                              evalPol b2;
                          evalPol (next);
          | While(c,b) ->
                          while(checkCond c) do
                            evalPol b
                          done;
                          evalPol (next);;
(*ajoute un élément à la hashmap*)
let rec addSousElem(expression:expr) = 
  match expression with
    | Num(i) -> ()
    | Var(n) -> Hashtbl.replace map_allVals n 1;
                Hashtbl.remove map_allVals "=";
    | Op(op,exp1,exp2) -> addSousElem(exp1);
                          addSousElem(exp2)
let addElemFromCond(c:cond)=
  let (b1,comparasion,b2) = c in
  addSousElem(b1);
  addSousElem(b2);;
(*Permet de vérifier quels variables sont initialisées et ne le sont pas*)
let rec var_polishBlock(blocka:block): unit = 
  match blocka with
  | [] ->();
  | (v,i)::next -> match i with
          |Set(n,e) ->
                      Hashtbl.replace map_allVals n 1;
                      Hashtbl.replace map_errVals n 1;

                      addSousElem(e);
                      var_polishBlock (next);
          | Read(n) ->
                      Hashtbl.replace map_errVals n 1;
                      Hashtbl.replace map_allVals n 1;
                      var_polishBlock (next);
          | Print(e) -> addSousElem(e);
                        var_polishBlock (next);
          | If(c,b1,b2) ->
                        addElemFromCond(c);
                        var_polishBlock b1;
                        var_polishBlock b2;
                        var_polishBlock (next)
          | While(c,b) ->
                        addElemFromCond(c);
                        var_polishBlock b;
                        var_polishBlock (next);;
let printamap(argc) = Hashtbl.iter (fun x y -> myprint x )argc;;
let getotherlist(valarg) = 
  let newTable = Hashtbl.create 0 in();
  Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" x y) newTable;
  ;;
let rec getValExprsBis (valExpr:expr) : expr = 
  match valExpr with
    | Num(a) -> Num(a)
    | Var(a) ->  if Hashtbl.mem my_hash a then Num( Hashtbl.find my_hash a) else Var(a)
    | Op(op1,ex1,ex2)-> calculB op1 ex1 ex2;
    and
    calculB op1 ex1 ex2=
      match op1 with
        | Add ->  Num(getVal1 ex1 + getVal1 ex2);
        | Sub ->  Num(getVal1 ex1 - getVal1 ex2);
        | Mul ->  Num(getVal1 ex1 * getVal1 ex2);
        | Div ->  Num(getVal1 ex1 / getVal1 ex2);
        | Mod ->  Num((getVal1 ex1) mod (getVal1 ex2))
let var_polish (p:program) : unit = var_polishBlock p;
                                    printf("Liste des variables utilisées : ");
                                    printamap(map_allVals);
                                    printf("\n");
                                    printf("Liste des variables utilisées : ");
                                    getotherlist(map_errVals);
                                    printamap(map_errVals);
                                    printf("\n");;
let simpl_exp(e:expr) : expr= getValExprsBis (e);;
let simpl_cond(c:cond) : cond = let (ex1,cmp1,ex2) = c in (simpl_exp(ex1),cmp1,simpl_exp(ex2));;
let bisTable = Hashtbl.create 0;;
(*Permet de simplifier toutes les expressions présentes dans le code*)
let rec simpl_polish (p:program) (p2:program): program=
  match p with 
  |[]-> p2
  |(i,ins)::ps -> match ins with
                  |Set(n,e) -> Hashtbl.replace my_hash n (getValExprs(e));
                              simpl_polish ps ( p2@[(i,(Set(n,(simpl_exp e))))])
                  |Read(n)  ->  Hashtbl.replace my_hash n 1;
                               simpl_polish ps (p2@[(i,Read(n))])
                  |Print(e) -> simpl_polish ps (p2@[(i,Print(simpl_exp e))])
                  |While(c,b) ->  (let c1=simpl_cond c in 
                                  let b1=simpl_polish b [] in 
                                  match c1 with 
                                  |(exp1,comp,exp2 )->  simpl_polish ps (p2@[(i,While((exp1,comp,exp2),b1))])
                                  )
                  |If(c,b1,b2)->  let bs1=simpl_polish b1 [] in
                                  let bs2=simpl_polish b2 [] in 
                                  let c1= simpl_cond c in
                                   match c1 with 
                                  |(exp1,comp,exp2) ->  simpl_polish ps (p2@[(i,If((exp1,comp,exp2),bs1,bs2))])                 
                  ;;
let rec addSousElem(expression:expr) = 
  match expression with
    | Num(i) -> ()
    | Var(n) -> Hashtbl.replace map_allVals n 1;
                if(Hashtbl.mem map_errVals n) then () else Hashtbl.replace bisTable n 1;
                Hashtbl.remove map_allVals "=";
    | Op(op,exp1,exp2) -> addSousElem(exp1);
                          addSousElem(exp2)
let addElemFromCond(c:cond)=
  let (b1,comparasion,b2) = c in
  addSousElem(b1);
  addSousElem(b2);;

let rec var_polishBlock(blocka:block): unit = 
  match blocka with
  | [] ->();
  | (v,i)::next -> match i with
          |Set(n,e) ->
                      Hashtbl.replace map_allVals n 1;
                      Hashtbl.replace map_errVals n 1;
                      addSousElem(e);
                      var_polishBlock (next);
          | Read(n) ->
                      Hashtbl.replace map_errVals n 1;
                      Hashtbl.replace map_allVals n 1;
                      var_polishBlock (next);
          | Print(e) -> addSousElem(e);
                        var_polishBlock (next);
          | If(c,b1,b2) ->
                        addElemFromCond(c);
                        var_polishBlock b1;
                        var_polishBlock b2;
                        var_polishBlock (next)
          | While(c,b) ->
                        addElemFromCond(c);
                        var_polishBlock b;
                        var_polishBlock (next);;
let printamap(argc) = Hashtbl.iter (fun x y -> myprint x )argc;
                      printf("\n");;
let getotherlist(valarg) = 
  let newTable = Hashtbl.create 0 in();
  Hashtbl.iter (fun x y -> if(Hashtbl.mem map_errVals x) then () else Hashtbl.replace newTable x 1;) map_allVals;
  newTable;
  ;;
let var_polish (p:program) : unit = var_polishBlock p;
                                    printf("Liste des variables utilisées : ");
                                    printamap(map_allVals);
                                    printf("\n");
                                    printf("Liste des variables non initialisées : ");
                                    printamap(bisTable);
                                    printf("\n");;


let print_polish (p:program) : unit = print_polishBlock p 0

let eval_polish (p:program) : unit = evalPol(p);;
let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish(read_polish(file))
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | [|_;"-simpl";file|] -> print_polish (simpl_polish(read_polish file)([]))
  | [|_;"-vars";file|] -> var_polish (read_polish file)

  | _ -> usage()

(* lancement de ce main *)
let () = main ()