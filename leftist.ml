(* Złączalna kolejka priorytetowa *)
(* zaimplementowana jako drzewo lewicowe *)
(* Węzeł ma dwa warianty: Null albo *)
(* Node (wartość, długość prawej ścieżki, lewe poddrzewo, prawe poddrzewo *)
type 'a queue =
	| Null
	| Node of 'a * int * 'a queue * 'a queue;;

(* Tworzy pustą kolejkę priorytetową *)
let empty = Null;;

(* Wyjątek zwracany, jeśli kolejka z której *)
(* usuwamy najmniejszy element jest pusta   *)
exception Empty

(* Sprawdza, czy kolejka que jest pusta *)
let is_empty (que : 'a queue) = que = Null;;

(* Funkcja porównująca dwa elementy kolejki *)
let compare (x : 'a) (y : 'a) = x < y;;

(* Zwraca wartość w pierwszym wierzchołku kolejki que *)
let frontOf (que : 'a queue) =
	match que with
	| Null -> raise (failwith "frontOf failure")
	| Node(v,_,_,_) -> v;;

(* Zwraca wysokość drzewa que *)
let heightOf (que : 'a queue) =
	match que with
	| Null -> 0
	| Node(_,h,_,_) -> h;;

(* Łączy kolejki queA i queB w jedną *)
let rec join (queA : 'a queue) (queB : 'a queue) =
	(* Jeśli jedna kolejka jest pusta, to zwraca drugą niezmienioną *)
	if is_empty queA then queB else if is_empty queB then queA
	(* queL i queR to kolejki o mniejszym i większym elemencie *)
	(* według funkcji compare *)
	else let queL, queR =
			if compare (frontOf queA) (frontOf queB)
			then (queA, queB) else (queB, queA) in
				match queL with
				| Null -> raise (failwith "join failure")
				| Node(vL, hL, lL, rL) ->
					(* newR to drzewo powstałe z połączenia słabszego drzewa *)
					(* z prawym poddrzewem mocniejszego drzewa               *)
					let newR = join rL queR in
					let hR = heightOf newR in
						(* Jeśli wysokość nowego prawego drzewa jest większa *)
						(* niż lewego, to trzeba je zamienić miejscami       *)
						if hR > hL then Node(vL, hL + 1, newR, lL)
						else 			Node(vL, hR + 1, lL, newR);;  

(* Dołącza element v do kolejki que *)
let add (v : 'a) (que : 'a queue) =
	join que (Node(v, 1, empty, empty));;

(* Usuwa najmniejszy element z kolejki i go zwraca *)
let delete_min (que : 'a queue) =
	match que with
	| Null -> raise Empty
	| Node(v, h, l, r) -> (v, join l r);;

