#load "graphics.cma";;
#load "unix.cma";;

(* The picks are represented as integer, 0 would be the leftmost pick
and 1 the first pick that is at the right of the leftmost pick, etc... *)
type picks = int;;

module Stack =
struct
	type 'a t = {mutable elements : 'a list }
	let create () = { elements = [] }
	let push x s = s.elements <- x :: s.elements
	let pop s =
		match s.elements with
            | h::t -> s.elements <- t; h
            | [] -> failwith "Empty stack"
    let underlying_data s = s.elements
    let length s = List.length s.elements
end;;

let number_discs = 5;;
let number_picks = 3;;

let arrays = [|Stack.create ();Stack.create ();Stack.create ()|];;

for i = number_discs downto 1 do
    Stack.push i arrays.(0);
done;;

let redrawState () =
    Graphics.clear_graph ();
    drawPicks ();
    drawDiscs arrays;
    Unix.sleep time_between_frame
;;

let moves_number = ref 0;;
let increment_moves_number () = moves_number := !moves_number + 1;;

let moveDisc origin destination =
    begin
		Stack.push (Stack.pop arrays.(origin)) arrays.(destination);
        increment_moves_number ();

		print_string "Move a disc from ";
        print_int origin;
        print_string " to ";
        print_int destination;
        print_newline ()
    end
;;

let hanoi n origin destination  =
    if n = 1 then
        moveDisc i j
	else
        begin
            hanoi (n - 1) i k j;
            moveDisc i j;
            hanoi (n - 1) k j i
        end
;;

Graphics.open_graph " 1280*720-0+0";;
hanoi number_discs 0 2 1;;
