
(* ----- IMPLEMENTAZIONE DI MINIMAX ----- *)

exception EmptyList;;

let rec lista_max = function
  (** Data una lista di elementi (a,b),
    restituisce l'elemento tale che b � massimo.
    Solleva l'eccezione EmptyList se la lista vuota.*)
  [] -> raise EmptyList
  | x::[] -> x
  | x::y::rest -> if (snd x) > (snd y) then lista_max (x::rest)
                  else lista_max (y::rest);;


let rec lista_min = function
  (** Data una lista di elementi (a,b),
   restituisce l'elemento tale che b � minimo
   Solleva l'eccezione EmptyList se la lista vuota. *)
  [] -> raise EmptyList
  | x::[] -> x 
  | x::y::rest -> if (snd x) < (snd y) then lista_min (x::rest)
                  else lista_min (y::rest);;


let minimax stato utility terminale succ_stato max =
  (** Implementazione dell'algoritmo MINIMAX.
      Parametri:
      - stato : stato del gioco dal quale cominciare la ricerca
      - utility : funzione di utilita' degli stati terminali 
      - terminale : funzione di determinazione degli stati terminali
      - succ_stato : funzione che restituisce i successori di uno stato
      - max : uguale a true se l'algoritmo deve massimizzare il risultato, false altrimenti *) 
  let rec massimizza stato =
    if terminale stato then
      (stato, utility stato)

    else lista_max (List.map minimizza (succ_stato stato))
  and minimizza stato =
    if terminale stato then
      (stato, utility stato)

    else lista_min (List.map massimizza (succ_stato stato))
  in match max with 
    true -> massimizza stato
    | false -> minimizza stato ;;


(* --- IMPLEMENTAZIONE DI HEX --- *)

type 'a grafo = Grafo of ('a -> 'a list) ;;

type colore = BIANCO | NERO ;;

    
(** Lo Stato in un particolare momento del gioco
 puo' essere descritto come una tupla di 6 elementi: 
    
   < campo, s, t, nodi bianchi, nodi neri, colore del prossimo giocatore > 
*)
type 'a stato = Stato of 'a grafo * 'a * 'a * 'a list * 'a list * colore ;;


let soluzione (Grafo succ) s t colorati =
  (** Esegue una visita in ampiezza di un grafo a partire da un nodo s, 
   considerando come vicini solo i nodi inclusi nella lista colorati.
   Se il nodo t viene visitato l'esecuzione termina e la funzione restituisce true,
   altrimenti restituisce false. *)
  let rec _sol visitati = function
    [] -> false
    | n::rest -> if List.mem n visitati then _sol visitati rest
                 else if n = t then true
                 else _sol (n::visitati)
                           (rest @ (List.filter
                   (function x -> List.mem x (t::colorati))
                                   (succ n)))
  in match colorati with
    [] -> false
    | _::_ -> _sol [] [s] ;;


let nodi_liberi (Grafo succ) s t colorati =
  (** Esegue una visita in ampiezza di un grafo a partire dal nodo s 
   e restituisce le lista dei nodi diversi da s e da t 
   e che non sono inclusi nella lista colorati. *)
  let rec _aux visitati liberi = function
    [] -> liberi
    | x::rest -> if List.mem x visitati then
                   _aux visitati liberi rest
             else if List.mem x (s::t::colorati) then
                   _aux (x::visitati) liberi (rest @ (succ x))
             else _aux (x::visitati) (x::liberi) (rest @ (succ x))
  in
    _aux [] [] [s] ;;


let test_terminale (Stato(grafo, s,t, nodi_bianchi, nodi_neri, prossimo)) =
  (** Dato uno Stato, restituisce true se si tratta di uno stato terminale,
    false altrimenti. Uno Stato  terminale quando:
    1) tutti i nodi del grafo sono stati colorati (eccetto i nodi s e t)
    2) uno dei due giocatori ha completato un percorso da s a t. *)  
  match nodi_liberi grafo s t (nodi_bianchi @ nodi_neri) with
    [] -> true
    | _::_ -> match prossimo with
                  BIANCO -> soluzione grafo s t nodi_neri
                  | NERO -> soluzione grafo s t nodi_bianchi;;


let succ_stato (Stato(grafo, s, t, nodi_bianchi, nodi_neri, prossimo)) =
  (** Dato uno Stato, restituisce la lista dei suoi possibili successori. *)
  let liberi = nodi_liberi grafo s t (nodi_bianchi @ nodi_neri) in
  match prossimo with
    BIANCO -> List.map 
    (function libero -> Stato(grafo, s, t, (libero::nodi_bianchi), 
                        nodi_neri, NERO)) liberi
    | NERO -> List.map
    (function libero -> Stato(grafo, s, t, nodi_bianchi,
                        (libero::nodi_neri), BIANCO)) liberi;;


(* ----- TEST ----- *)

let utility(Stato(campo, s,t, nodi_bianchi, nodi_neri, prossimo)) =
  (** Funzione di utilit�  dell'algoritmo minimax 
      -> 1 = BIANCO vince
      -> -1 = NERO vince
      -> 0 = pareggio
  *)
  if soluzione campo s t nodi_bianchi then 1
  else if soluzione campo s t nodi_neri then -1  
  else 0;;  


let test_minimax (Stato (campo, s, t, nb, nn, colore)) =
  (** Esegue l'algoritmo minimax a partire da un dato Stato 
   e restituisce lo Stato finale migliore calcolato, 
   il suo valore ed il tempo totale di esecuzione*)

  let start_time = Sys.time() in
    let (finale, valore) = minimax (Stato (campo, s, t, nb, nn, colore)) utility test_terminale succ_stato (colore = BIANCO) in
      (finale, valore, Sys.time() -. start_time);;


let hex_succ (righe, colonne) (y,x) =
  let valido (i,j) =
    (i >= 0) && (i < righe) &&
    (j >= 0) && (j < colonne)
  in let vicini = [ 
    (y, x - 1); (y, x + 1);
    (y - 1, x); (y + 1, x);
    (y - 1, x + 1); (y + 1, x - 1)]
  in
   List.filter valido vicini ;;


let dim = 3 ;;

let grafo_hex = Grafo (hex_succ (dim, dim));;

let s = (0,0);; 
let t = (dim - 1, dim - 1);;
let init = Stato (grafo_hex, s, t, [], [], BIANCO);;

(* test_minimax init;; *)
