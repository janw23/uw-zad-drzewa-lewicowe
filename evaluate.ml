open Leftist

let compare (x : 'a) (y : 'a) = x < y;;
let compare_return_int x y = if compare x y then 0 else 1;;

let random_range n r =
	let rec iterate k acc =
		if k = 0 then acc
		else iterate (k - 1) ((Random.int r) :: acc)
	in iterate n [];; 

let str_list lst =
	let rec helper l acc =
		match l with
		| [] -> acc ^ "]"
		| h :: [] -> helper [] (acc ^ (string_of_int h))
		| h :: t -> helper t (acc ^ (string_of_int h) ^ "; ")
	in helper lst "[";;

let str_queue que =
	let rec helper q acc =
		if is_empty q then acc ^ "]"
		else let v, nq = delete_min q in
			helper nq (acc ^ (string_of_int v) ^ (if is_empty nq then "" else "; "))
	in helper que "[";;  

let list_to_queue lst =
	let rec helper l q =
		match l with
		| [] -> q
		| h :: t -> helper t (add h q)
	in helper lst empty;;

let a = random_range 10 100;;
let b = List.sort compare_return_int a;;
let q = list_to_queue a;;

print_string ("list = " ^ (str_list a));;
print_string "\n";
print_string ("sorted list = " ^ (str_list b));;
print_string "\n";;
print_string ("queue from list = " ^ (str_queue q));;
print_string "\n";;

let print msg = print_string msg;;
let println msg = print (msg ^ "\n");;

let run_test n r =
	let rec iterate k =
		if k = 0 then () else (
		println ("Running test #" ^ (string_of_int (n - k)));
		let a = random_range (Random.int 10 + 10) r
		and b = random_range (Random.int 10 + 10) r
		in
			let sorted_list = List.sort compare_return_int (a @ b)
			and que = list_to_queue (a @ b)
			in
				println ("List  = " ^ (str_list sorted_list));
				println ("Queue = " ^ (str_queue que));
				assert((str_queue que) = (str_list sorted_list));
				println "";
				iterate (k - 1))
	in iterate n;;

run_test 1000 1000;;