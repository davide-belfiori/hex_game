(* Esempi di applicazione dell'algoritmo minimax *)

open Minimax;;
open Hexgame;;
open Grafo;;
open Console;;

let utility (Stato(campo, s, t, nodi_bianchi, nodi_neri, prossimo)) =
  (** Funzione di utilità dell'algoritmo minimax 
      -> 1 = BIANCO vince
      -> -1 = NERO vince
      -> 0 = pareggio
  *)

  if soluzione campo s t nodi_bianchi then 1
  else if soluzione campo s t nodi_neri then -1   
  else 0;;


let stampa_valore = function
      1 -> print_endline ("Valore dello stato = 1 -> VITTORIA")
    | 0 -> print_endline ("Valore dello stato = 0 -> PAREGGIO")
    | -1 -> print_endline ("Valore dello stato = -1 -> SCONFITTA")
    | _ -> ();;


let test_minimax (Stato (campo, s, t, nb, nn, colore)) =
  (** Esegue l'algoritmo minimax a partire da un dato Stato 
   e restituisce lo Stato finale migliore calcolato, 
   il suo valore ed il tempo totale di esecuzione*)

  let start_time = Sys.time() in
    let (finale, valore) = minimax (Stato (campo, s, t, nb, nn, colore)) utility test_terminale succ_stato (colore = BIANCO) in
      (finale, valore, Sys.time() -. start_time);;

(* ----- TEST 1 ----- *)

let campo_hex_3_3 = 
"   0   1   2
 0 S - O - O    
    \\ / \\ / \\   
   1 O - O - O      
      \\ / \\ / \\     
     2 O - O - T"

let campo1 = nuovo_campo_hex 3 3;;

let run_test_1 () =
  (** Test dell'algoritmo minimax applicato al classico
      campo da gioco di hex a partire dallo stato iniziale 
      del gioco.
     
        0   1   2
      0 S - O - O
         \ / \ / \
        1 O - O - O
           \ / \ / \
          2 O - O - T
  
      I nodi s e t corrispondono alle coordinate (0,0) e (2,2). 
      Questa particolare configurazione ammette una strategia 
      VINCENTE per il giocatore che effettua la prima mossa. *)

  let (finale, valore, ex_time) = test_minimax (Stato (campo1, (0,0), (2,2), [], [], BIANCO))
    in 
      print_endline ("\n--- TEST 1 --- \n\n" ^ campo_hex_3_3 ^ "\n\n --- SOLUZIONE OTTIMA --- \n");
      stampa_stato finale string_of_int_tuple; 
      stampa_valore valore;
      print_endline ("Tempo di esecuzione (secondi) = " ^ (string_of_float ex_time));;


(* ----- TEST 2 ----- *)

let stringa_campo2 =
"  A - C ---- G
  | / | \\  /  \\ 
  S - E   F -- I 
  | \\ |    \\  / 
  B - D      T"

let succ_campo2 = function
    "S" -> ["A";"B";"C";"D";"E"]
    | "A" -> ["S";"C"]
    | "B" -> ["S";"D"]
    | "C" -> ["S";"A";"E";"F";"G"]
    | "D" -> ["S";"B";"E";]
    | "E" -> ["S";"C";"D";]
    | "F" -> ["C";"G";"I";"T"]
    | "G" -> ["F";"I";"C"]
    | "T" -> ["F";"I"]
    | "I" -> ["F";"G";"T"]
    | _ -> [];;

let campo2 = Grafo (succ_campo2)

let run_test_2 () =
  (** Test dell'algoritmo minimax applicato ad un
      campo da gioco generico.
     
        A - C ---- G
        | / | \  /  \ 
        S - E   F -- I
        | \ |    \  / 
        B - D      T

      Questa particolare configurazione non garantisce una strategia 
      VINCENTE per il giocatore che effettua la prima mossa, 
      il risultato migliore ottenibile è il PAREGGIO. *)

  let (finale, valore, ex_time) = test_minimax (Stato (campo2, "S", "T", [], [], BIANCO))
    in 
      print_endline ("\n --- TEST 2 --- \n\n" ^ stringa_campo2 ^ "\n\n --- SOLUZIONE OTTIMA --- \n");
      stampa_stato finale (function s -> s);
      stampa_valore valore;
      print_endline ("Tempo di esecuzione (secondi) = " ^ (string_of_float ex_time));;


(* ----- TEST 3 ----- *)

let stringa_campo3 =
"       10
      / | \\    
    1 - 4 - 7   
   / \\ / \\ / \\
  0 - 2 - 5 - 8
   \\ / \\ / \\ /
    3 - 6 - 9"

let archi_campo3 = [(0,1); (0,2); (0,3); 
                    (1,2); (1,4); 
                    (2,4); (2,5); (2,6); (2,3); 
                    (3,6); 
                    (4,7); (4,5); 
                    (5,6); (5,7); (5,8); (5,9);
                    (6,9); 
                    (7,8); 
                    (8,9);
                    (1,10); (4,10); (7,10)];;

let succ_campo3 archi nodo =
    let rec _succ = function
        [] -> []
        | (x,y)::rest -> if x = nodo then y::(_succ rest)
                         else if y = nodo then x::(_succ rest)
                         else _succ rest
    in _succ archi;;

let campo3 = Grafo (succ_campo3 archi_campo3);;

let run_test_3 () =
  (** Test dell'algoritmo minimax applicato ad un
      campo da gioco generico.
     
            10
           / | \ 
         1 - 4 - 7   
        / \ / \ / \
       0 - 2 - 5 - 8
        \ / \ / \ /
         3 - 6 - 9

      I nodi s e t corrispondono a 0 ed 8.
      Questa particolare configurazione non garantisce una strategia 
      VINCENTE per il giocatore che effettua la prima mossa, 
      il risultato migliore ottenibile è il PAREGGIO. *)

  let (finale, valore, ex_time) = test_minimax (Stato (campo3, 0, 8, [], [], BIANCO))
    in 
      print_endline ("\n --- TEST 3 --- \n\n" ^ stringa_campo3 ^ "\n\n --- SOLUZIONE OTTIMA --- \n");
      stampa_stato finale string_of_int; 
      stampa_valore valore;
      print_endline ("Tempo di esecuzione (secondi) = " ^ (string_of_float ex_time));;


let () = run_test_1(); 
         run_test_2(); 
         run_test_3();
