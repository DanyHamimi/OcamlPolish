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

(*quand t as un if t'ouvres nouveau bloc et tu lis les instructions après ca commence par un if et ca se termine 
1: n
2: lt n 0 (1 val Sub 0 n) (n)
3: n*)
(***********************************************************************)

                 

let myread (name:string) : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []



(*let create_blocs (name:string list) : program = let program = 
let block = (let position = 1 let instr = name);;*)



let read_polish (filename:string) : program = failwith "TODO"

let print_polish (p:program) : unit = failwith "TODO"

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let myprint s =  printf "%s " s



let getListWord s = String.split_on_char (' ') s (*list de tout les mots de s*)
let printList (s) = List.iter (myprint) (getListWord(s))
let rec print_list_string myList i= match myList with
  | [] -> print_endline "Fin de fichier"
  | head::body -> 
    begin
    printList head;
    printf "\n";
    print_list_string (body) (i+1)
    end
    ;;
let  createprgm myList i = match myList with
    | [] -> print_endline "Programme crée"
    | "COMMENT" ::  s -> print_string "COMMENTAIRE" (*TODO GO UNTIL NEXT EXPRESSION HORS DU COMMENTAIRE*)
    | "READ" :: s -> print_string "c'est read" (*Creer une var de type Read of name*)
    | "IF" :: s -> print_string "c'est if" (*Creer un nouveau bloc contenant la condition de comparaison + les 2 blocs à effectuer*)
    | "ELSE" :: s -> print_string "c'est else"
    | "WHILE" :: s -> print_string "c'est while"

    | _ -> printf "to"
let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_list_string(myread(file)) (1)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | _ -> usage()

(* lancement de ce main *)
let () = main ()

(*TODO Delete all comments with a function.*)