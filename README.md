# MinimHex
Implementazione funzionale dell'algoritmo minimax applicato alla ricerca di una strategia ottima del gioco Hex.

## Descrizione del problema
Dato un grafo G=(V,E) e due vertici iniziali s, t si consideri il seguente gioco: I due giocatori (Bianco e Nero) scelgono alternativamente un vertice in V-{s,t} e lo colorano del proprio colore. Bianco vince se e solo se riesce a realizzare un cammino da s a t di vertici bianchi. 

Esiste una strategia vincente per il giocatore Bianco? 

## Ricerca della strategia vincente
L'algoritmo minimax permette il calcolo della strategia ottima applicabile da un giocatore a partire da un particolare stato del gioco.

La ricerca avviene mediante l'esplorazione di un albero composto da tutti i possibli successori dello stato iniziale, l'algoritmo sceglie la strategia che massimizza/minimizza il risultato per il dato giocatore.

Oltre allo stato di partenza, minimax richiede:
- una funzione di utilità che assegna un valore ad uno stato terminale del gioco
- una funzione di test per determinare se uno stato è terminale
- una funzione per il calcolo dei successori di uno stato
- indicare se l'algoritmo deve massimizzare o minimizzare il risultato

Questo gioco, al contrario della versione classica di Hex, ammette che la partita termini con un pareggio e, in base al grafo preso in considerazione, non è sempre garantita l'esistenza di una strategia vincente per il giocatore Bianco.

## Compilare ed eseguire l'applicazione

> **Prerequisiti** : GNU-Make in versione 3.80 o maggiore.

1. Clonare la repository
```
$ git clone https://github.com/davide-belfiori/minimhex
$ cd minimhex
```

2. Compilare ed eseguire
```
$ make
$ ./minimhex
```
