(* The picks are represented as integer, 0 would be the leftmost pick
and 1 the fist pick that is at the right of the leftmost pick, etc... *)
type picks = int;;

module Stack =
struct
	type 'a t = {mutable elements : 'a list }
	let create () = { elements = [] }
	let push x s = s.elements <- x :: s.elements
	let pop s =
		match s.elements with
		h::t -> s.elements <- t; h
		| [] -> failwith "Empty stack"
end;;

let arrays = [|Stack.create ();Stack.create ();Stack.create ()|];;

for i = 5 downto 1 do  Stack.push i arrays.(0); done;;


let moves_number = ref 0;;
let increment_moves_number () = moves_number := !moves_number + 1;;

let moveDisc origin destination =
    begin
		Stack.push (Stack.pop arrays.(destination)) arrays.(origin);
        increment_moves_number ();

        print_string "Move a disc from ";
        print_int origin;
        print_string " to ";
        print_int destination;
        print_newline ()
    end;;

let rec hanoi n i j k =
    if n = 1 then moveDisc i j
	else
		hanoi (n-1) i k j;
		moveDisc i j;
		hanoi (n-1) k j i;;

print_string "Move a disc from ";
	
hanoi 5 0 2 1;;
