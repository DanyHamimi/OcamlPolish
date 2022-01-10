open Printf

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

  let retBod (s: string list): string list =
    match s with
      |[]->[]
      |head::body->body