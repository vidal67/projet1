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

let add_points point1 point2 =
    let x1, y1 = point1 in
    let x2, y2 = point2 in
    (x1 + x2, y1 + y2)
;;

let drawFilledPolygon points color = 
    (* Draw in the given color the polygons represented by points *)
	Graphics.set_color color;
	Graphics.fill_poly points
;;

let number_picks = 3;;
let picks_periode = 100;;
let picks_heights = 100;;
let number_discs = 5;;
let disc_minimum_width = 10;;
let disc_maximum_width = picks_periode / 2;;
let disc_height = 100 / (number_discs + 1);;
let picks_width = disc_minimum_width / 2;;

let picks_color = Graphics.black;;

let getNemeDiscWidth n =
    (* The disc numbers start at 0 *)
    let range = disc_maximum_width - disc_minimum_width in
    let width_slope =  range / (number_discs - 1) in
    (n - 1) * width_slope + disc_minimum_width
;;
    
let getNemeDiscVertices n =
    (* Return the vertices of the rectangle that represent the neme disc
        if the origin were at the middle of the bottom edge *)
    let disc_width = getNemeDiscWidth n in
    [|  -disc_width / 2, 0;
        -disc_width / 2, disc_height;
        disc_width / 2, disc_height;
        disc_width / 2, 0 |]
;;

let drawNemeDiscAtPosition n position =
    let vertices = Array.map (add_points position) (getNemeDiscVertices n) in
    Graphics.fill_poly vertices
;;

let drawStackOfDisc stack bottom_position =
    let rec aux current_position discs =
        match discs with
            | [] -> ()
            | disc::rst_discs ->
                let next_position = add_points current_position (0, -disc_height) in
                drawNemeDiscAtPosition disc current_position;
                aux next_position rst_discs
    in
    (* We retrieve the position of the top disc so that it is easier to
        draw the stack of disc: from top to bottom *)
    let top_position_y = (Stack.length stack - 1) * disc_height in
    let top_position = add_points bottom_position (0, top_position_y) in
    (* We want to work on the underlying list of the stack, because we
        want to iterate on each element without modifying the stack state *)
    aux top_position (Stack.underlying_data stack)
;;

let getPickPosition pick = (picks_periode / 2 + picks_periode * pick, 0);;

let drawPicks () =
    let picks_vertices = [|
        0, 0;
        picks_width, 0;
        picks_width, picks_heights;
        0, picks_heights |] in 
    for pick = 0 to number_picks - 1 do
        let add_picks_position = add_points (getPickPosition pick) in
        let current_picks = Array.map add_picks_position picks_vertices in
        drawFilledPolygon current_picks picks_color
    done
;;

let drawDiscs stacks =
    (* Draw the disc contained in each stack of the array `stacks` *)
    for pick = 0 to Array.length stacks - 1 do
        let pick_position = getPickPosition pick in
        let bottom_position = add_points pick_position (picks_width / 2, 0) in
        drawStackOfDisc stacks.(pick) bottom_position
    done
;;


let arrays = [|Stack.create ();Stack.create ();Stack.create ()|];;

for i = number_discs downto 1 do
    Stack.push i arrays.(0);
done;;

let time_between_frame = 1 (* In seconds *);;

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
        redrawState ();

        print_string "Move a disc from ";
        print_int origin;
        print_string " to ";
        print_int destination;
        print_newline ()
    end
;;

let rec hanoi n i j k =
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
drawPicks ();;
hanoi number_discs 0 2 1;;
Unix.sleep 10;;
