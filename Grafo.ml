(*
 Implementazione del Grafo
*)
 
(** Un grafo è rappresentato da una funzione successore, 
  ovvero una funzione che, dato un vertice del grafo restituisce tutti i suoi vicini.*)
type 'a grafo = Grafo of ('a -> 'a list);;