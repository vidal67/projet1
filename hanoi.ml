(* The picks are represented as integer, 0 would be the leftmost pick
and 1 the fist pick that is at the right of the leftmost pick, etc... *)
type picks == int;;

let moves_number = ref 0;;
let increment_moves_number () = moves_number := !moves_number + 1;;

let moveDisc origin destination =
    begin
        increment_moves_number ();

        print_string "Move a disc from ";
        print_int origin;
        print_string " to ";
        print_int destination;
        print_newline ()
    end
;;
