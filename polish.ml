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

                 

let myread (name:string) : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []



let t = "testest"

let read_polish (filename:string) : program = failwith (t)

let print_polish (p:program) : unit = failwith "TODO"

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let myprint s =  printf "%s " s





let getListWord s = String.split_on_char (' ') s (*list de tout les mots de s*)
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
  
let rec getFristCmd l j= match l with
  | [] -> print_endline "-ligne vide-"
  | head::body -> 
    begin
      match head with
        |" " -> getFristCmd (body) (j+1)
        |"" -> getFristCmd body (j+1)
        | _ -> printf "%s                   nombre d'espace: %i" head (j/2)
    end
    ;;


let printList (s) = List.iter (myprint) (getListWord(s))
let rec print_list_string myList i= match myList with
  | [] -> print_endline "Fin de fichier"
  | head::body -> 
    begin
    printf "%i: " i;
    (*cmd(printFirstWord (head));*)
    getFristCmd (getListWord(head)) (0);
    printf "\n";
    print_list_string (body) (i+1)
    end
    ;;

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_list_string(myread(file)) (1)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | _ -> usage()

(* lancement de ce main *)
let () = main ()
