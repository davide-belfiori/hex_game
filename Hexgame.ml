(*
 Implementzione dei tipi e delle funzioni del gioco HEX
*)

open Grafo;;

(** Colore dei giocatori *)
type colore = BIANCO | NERO ;;

    
(** Lo Stato in un particolare momento del gioco
 puo' essere descritto come una tupla di 6 elementi: 
    
   < campo, s, t, nodi bianchi, nodi neri, colore del prossimo giocatore > 
*)
type 'a stato = Stato of 'a grafo * 'a * 'a * 'a list * 'a list * colore ;;


let soluzione (Grafo succ) s t colorati =
  (** Esegue una visita in ampiezza di un grafo a partire da un nodo s, 
   considerando come vicini solo i nodi inclusi nella lista colorati.
   Se il nodo t viene visitato l’esecuzione termina e la funzione restituisce true,
   altrimenti restituisce false.
  *)
  let rec _sol visitati = function
      [] -> false
      | n::rest -> if List.mem n visitati then _sol visitati rest
                   else if n = t then true
                   else _sol (n::visitati)
                             (rest @ (List.filter (function x -> List.mem x (t::colorati)) (succ n)))
    in match colorati with
      [] -> false
      | _::_ -> _sol [] [s] ;;



let nodi_liberi (Grafo succ) s t colorati =
  (** Esegue una visita in ampiezza di un grafo a partire dal nodo s 
   e restituisce le lista dei nodi diversi da s e da t 
   e che non sono inclusi nella lista colorati. 
  *)
    let rec _aux visitati liberi = function
        [] -> liberi
        | x::rest -> if List.mem x visitati then _aux visitati liberi rest
                else 
                    if List.mem x (s::t::colorati) then _aux (x::visitati) liberi (rest @ (succ x))
                else 
                    _aux (x::visitati) (x::liberi) (rest @ (succ x))
    in
        _aux [] [] [s] ;;



let test_terminale (Stato (grafo, s, t, nodi_bianchi, nodi_neri,prossimo)) =
  (** Dato uno Stato, restituisce true se si tratta di uno stato terminale,
    false altrimenti. Uno Stato  terminale quando:
    1) tutti i nodi del grafo sono stati colorati (eccetto i nodi s e t)
    2) uno dei due giocatori ha completato un percorso da s a t.
  *)
    match nodi_liberi grafo s t (nodi_bianchi @ nodi_neri) with
        [] -> true
        | _::_ -> match prossimo with
                    BIANCO -> soluzione grafo s t nodi_neri
                    | NERO -> soluzione grafo s t nodi_bianchi;;



let succ_stato (Stato (grafo, s, t, nodi_bianchi, nodi_neri, prossimo)) =
  (** Dato uno Stato, restituisce la lista dei suoi possibili successori.
   *)
    let liberi = nodi_liberi grafo s t (nodi_bianchi @ nodi_neri) in
    match prossimo with
        BIANCO -> List.map (function libero -> Stato(grafo, s, t, (libero::nodi_bianchi), nodi_neri, NERO)) liberi
        | NERO -> List.map (function libero -> Stato(grafo, s, t, nodi_bianchi, (libero::nodi_neri), BIANCO)) liberi;;


(* Definizione del campo da gioco standard di HEX *)

let hex_succ (rows, cols) (y,x) =
  (** Funzione successore di una casella in un
   campo da gioco standard di Hex
  *)
    let valido (i,j) = 
        (i >= 0) && (i < rows) &&
        (j >= 0) && (j < cols)
    in let vicini = [
        (y, x - 1); (y, x + 1);
        (y - 1, x); (y + 1, x);
        (y - 1, x + 1); (y + 1, x - 1) ]
    in 
        List.filter valido vicini;;

let nuovo_campo_hex righe colonne = 
  (** Dati il numero di righe e di colonne, restituisce
   il grafo rappresentante un campo da gioco di Hex.
  *)
    if righe > 1 && colonne > 1 then
        Grafo (hex_succ (righe, colonne))
    else 
        raise (Invalid_argument ("Dimensioni non consentite"));;
