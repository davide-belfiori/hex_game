(* 
  Implementazione delle funzioni di stampa a video
*)

open Hexgame;;

let rec string_of_list string_of separatore = function
  (** Data una lista ed una funzione di conversione in stringa,
    restituisce la stringa risultante dalla concatenazione di tutti gli
    elementi della lista convertiti in stringa [NON RICORSIVA DI CODA] *)

    [] -> ""
  | x::[] -> string_of x
  | x::rest -> (string_of x) ^ separatore ^ (string_of_list string_of separatore rest);;


let string_of_colore = function    
  (** Restituisce una stringa che rappresenta il colore di un gicatore *)

    BIANCO -> "BIANCO"
  | NERO -> "NERO"
  | _ -> "";;


let string_of_int_tuple (a,b) = 
  (** Restituisce una stringa che rappresenta una coppia di interi *)

  "(" ^ (string_of_int a) ^ "," ^ (string_of_int b) ^ ")"


let stampa_stato (Stato(campo, s, t, nodi_bianchi, nodi_neri, prossimo)) string_of_mossa = 
  (** Stampa a video le informazioni relative ad uno Stato di gioco *)

  let stringa = "Nodi bianchi : [" ^ (string_of_list string_of_mossa ", " nodi_bianchi) ^ "]\n" ^
                "Nodi neri : [" ^ (string_of_list string_of_mossa ", " nodi_neri) ^ "]\n" ^ 
                "Prossimo turno : " ^ (string_of_colore prossimo) ^ "\n" in
  print_string stringa;;
 
