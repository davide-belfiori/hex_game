(*
 Implementazione dell'algoritmo MINIMAX
*)

exception EmptyList;;

let rec lista_max = function
    (** Data una lista di elementi (a,b),
     restituisce l'elemento tale che b è massimo.
     Solleva l'eccezione EmptyList se la lista vuota. *)

    [] -> raise EmptyList
    | x::[] -> x
    | x::y::rest -> if (snd x) > (snd y) then lista_max (x::rest)
                  else lista_max (y::rest);;


let rec lista_min = function
    (** Data una lista di elementi (a,b),
     restituisce l'elemento tale che b è minimo
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
