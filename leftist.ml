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
	| Node(v,_,_,_) -> v
	| Null -> raise Empty;;

(* Zwraca wysokość drzewa que *)
let heightOf (que : 'a queue) =
	match que with
	| Node(_,h,_,_) -> h
	| Null -> 0;;

(* Łączy kolejki queA i queB w jedną *)
let rec join (queA : 'a queue) (queB : 'a queue) =
	(* Jeśli jedna kolejka jest pusta, to zwraca drugą niezmienioną *)
	if is_empty queA then queB else if is_empty queB then queA
	(* queL i queR to kolejki o mniejszym i większym elemencie *)
	(* według funkcji compare *)
	else let queL, queR =
			if compare (frontOf queA) (frontOf queB)
			then (queA, queB) else (queB, queA)
		 in
			match queL with
			| Node(vL, _, lL, rL) ->
				(* newR to drzewo powstałe z połączenia słabszego drzewa *)
				(* z prawym poddrzewem mocniejszego drzewa               *)
				let newR = join rL queR in
				let hR = heightOf newR
				and hL = heightOf lL in
					(* Jeśli wysokość nowego prawego drzewa jest większa *)
					(* niż lewego, to trzeba je zamienić miejscami       *)
					if hR > hL then Node(vL, hL + 1, newR, lL)
					else 			Node(vL, hR + 1, lL, newR)
			| Null -> raise Empty;;

(* Dołącza element v do kolejki que *)
let add (v : 'a) (que : 'a queue) =
	join que (Node(v, 1, empty, empty));;

(* Zwraca parę (najmniejszy element, nowe drzewo bez tego elementu *)
let delete_min (que : 'a queue) =
	match que with
	| Node(v, _, l, r) -> (v, join l r)
	| Null -> raise Empty;;

let a = add 10 empty;;
let a = add 12 a;;
let a = add 5 a;;

let b = add 8 empty;;
let b = add 14 b;;
let b = add 7 b;;
let b = add 3 b;;

let c = join a b;;
let (e, c) = delete_min c;;
assert(e = 3);;

let (e, c) = delete_min c;;
assert(e = 5);;

let (e, c) = delete_min c;;
assert(e = 7);;

let (e, c) = delete_min c;;
assert(e = 8);;

let (e, c) = delete_min c;;
assert(e = 10);;

let (e, c) = delete_min c;;
assert(e = 12);;

let (e, c) = delete_min c;;
assert(e = 14);;

assert(is_empty c);;

assert( try let _ = delete_min c in false with Empty -> true);;

let b = add "a" empty;;
let b = add "aca" b;;
let b = add "nzbzad" b;;
let b = add "nzbza" b;;
let b = add "bxbxc" b;;

let (a,b) = delete_min b;;
assert (a = "a");;

let (a,b) = delete_min b;;
assert (a = "aca");;

let (a,b) = delete_min b;;
assert (a = "bxbxc");;

let (a,b) = delete_min b;;
assert (a = "nzbza");;

let (a,b) = delete_min b;;
assert (a = "nzbzad");;

assert(is_empty b = true);;
assert (try let _=delete_min b in false with Empty -> true);;