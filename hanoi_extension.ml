#load "graphics.cma";;
#load "unix.cma";;

(* The picks are represented as integer, 0 would be the leftmost pick
and 1 the first pick that is at the right of the leftmost pick, etc... *)
type picks = int;;

module Stack =
struct
	type 'a t = {mutable elements : 'a list }
	let create () = { elements = [] }
	let push x s = s.elements <- x :: s.elements let pop s =
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

(* Variables that configure the display *)
let should_display = true;;
let time_between_frame = 0.2 (* In seconds *);;
let picks_periode = 200;;
let picks_heights = 300;;
let disc_minimum_width = 10;;
let disc_maximum_width = picks_periode / 2;;
let picks_width = disc_minimum_width / 2;;
let picks_color = Graphics.black;;

let getDiscHeight number_discs = picks_heights / (number_discs + 1);;

let getNemeDiscWidth number_discs n =
    (* The disc numbers start at 0 *)
    let range = disc_maximum_width - disc_minimum_width in
    let width_slope =  range / (number_discs - 1) in
    (n - 1) * width_slope + disc_minimum_width
;;
    
let getNemeDiscVertices number_discs n =
    (* Return the vertices of the rectangle that represent the neme disc
        if the origin were at the middle of the bottom edge *)
    let disc_width = getNemeDiscWidth number_discs n in
    let disc_height = getDiscHeight number_discs in
    [|  -disc_width / 2, 0;
        -disc_width / 2, disc_height;
        disc_width / 2, disc_height;
        disc_width / 2, 0 |]
;;

let drawNemeDiscAtPosition number_discs n position =
    let base_vertices = getNemeDiscVertices number_discs n in
    let vertices = Array.map (add_points position) base_vertices in
    Graphics.fill_poly vertices
;;

let drawStackOfDisc pick_content number_discs bottom_position =
    let disc_height = getDiscHeight number_discs in
    let rec aux current_position discs =
        match discs with
            | [] -> ()
            | disc::rst_discs ->
                let next_position = add_points current_position (0, -disc_height) in
                drawNemeDiscAtPosition number_discs disc current_position;
                aux next_position rst_discs
    in
    (* We retrieve the position of the top disc so that it is easier to
        draw the pick_content of disc: from top to bottom *)
    let top_position_y = (Stack.length pick_content - 1) * disc_height in
    let top_position = add_points bottom_position (0, top_position_y) in
    (* We want to work on the underlying list of the pick_content, because we
        want to iterate on each element without modifying the pick_content state *)
    aux top_position (Stack.underlying_data pick_content)
;;

let getPickPosition pick = (picks_periode / 2 + picks_periode * pick, 0);;

let drawPicks number_picks =
    let picks_vertices = [|
        0, 0;
        picks_width, 0;
        picks_width, picks_heights;
        0, picks_heights |] in 
    for pick = 0 to number_picks - 1 do
        let add_picks_position = add_points (getPickPosition pick) in
        let current_picks_vertices = Array.map add_picks_position
                                               picks_vertices
        in
        drawFilledPolygon current_picks_vertices picks_color
    done
;;

let drawDiscs number_discs picks_content=
    (* Draw the disc contained in each stack of the array `picks_content` *)
    for pick = 0 to Array.length picks_content - 1 do
        let pick_position = getPickPosition pick in
        let bottom_position = add_points pick_position (picks_width / 2, 0) in
        drawStackOfDisc picks_content.(pick) number_discs bottom_position
    done
;;

let generatePicks number_picks =
    let pick_array = Array.make number_picks (Stack.create ()) in
    for i = 1 to number_picks - 1 do
        pick_array.(i) <- Stack.create ()
    done;
    pick_array
;;

let initializeHanoiTower number_picks number_discs =
    let picks_content = generatePicks number_picks in
    for i = number_discs downto 1 do
        Stack.push i picks_content.(0);
    done;
    picks_content
;;


let drawState number_discs picks_content =
    Graphics.clear_graph ();
    drawPicks (Array.length picks_content);
    drawDiscs number_discs picks_content;
;;

let getPicksList number_picks =
    let rec aux k =
        match k with
            | -1 -> []
            | k  -> k::aux (k - 1)
    in
    aux (number_picks - 1)
;;

let moves_number = ref 0;;
let increment_moves_number () = moves_number := !moves_number + 1;;

let moveDisc number_discs picks_content origin destination =
    begin
		Stack.push (Stack.pop picks_content.(origin)) picks_content.(destination);
        increment_moves_number ();
        if should_display then
            drawState number_discs picks_content;
        Unix.sleepf time_between_frame
    end
;;

let round f = int_of_float (f +. 0.5);;

let hanoi3Picks number_discs
                picks_content
                number_discs_to_move
                origin
                destination
                other =

    let rec aux number_discs_to_move origin destination other =
        if number_discs_to_move > 0 then
        begin
            aux (number_discs_to_move - 1) origin other destination;
            moveDisc number_discs picks_content origin destination;
            aux (number_discs_to_move - 1) other destination origin
        end
    in
    aux number_discs_to_move origin destination other
;;

let hanoi number_discs picks_content number_discs_to_move origin destination =
    (* This function must be called when `picks_content` has at least
        `number_discs_to_move` discs in 
        `origin` and no disc everywhere else otherwise its behaviour is
        undefined*)
    let getIntermediateDiscNumber number_discs_to_move number_picks =
        (* We use the best value of intermediate discs to move for a tower of
            Hanoi with 4 picks, because we don't want to search for the best
            value for other cases *)
        number_discs_to_move
            - round (sqrt (float_of_int (2 * number_discs_to_move + 1))) + 1
    in
    let rec aux number_discs_to_move origin destination free_picks =
        let free_picks_number = List.length free_picks in
        let intermediate_disc_number =
            getIntermediateDiscNumber number_discs_to_move free_picks_number in 

        (* If the size of the problem is too little i-e we can't move
            `intermediate_disc_number` from `origin` or we can't use
            more that one intermediate picks, we use this as our base
            case *)
        if List.length free_picks <= 1
                || number_discs_to_move <= intermediate_disc_number then
            hanoi3Picks number_discs
                        picks_content
                        number_discs_to_move
                        origin
                        destination
                        (List.hd free_picks)
        else

            (* Here, we use Frame-Stewart algorithm as it is believed to be
                optimal *)
            let intermediate_destination = List.hd free_picks in
            let intermediate_free_picks = destination::(List.tl free_picks) in
            begin
                aux intermediate_disc_number
                    origin
                    intermediate_destination
                    intermediate_free_picks;
                aux (number_discs_to_move - intermediate_disc_number)
                    origin
                    destination
                    (List.tl free_picks);
                aux intermediate_disc_number
                    intermediate_destination
                    destination
                    (origin::List.tl free_picks)
            end
    in
    let is_free = fun p -> p <> origin && p <> destination in
    let number_picks = Array.length picks_content in
    let free_picks = List.filter is_free (getPicksList number_picks) in
    aux number_discs_to_move origin destination free_picks
;;

if should_display then
    Graphics.open_graph " 1280*720-0+0";;

let number_discs = 10;;
let number_picks = 5;;
 
let file = "hanoi_extension.csv";;
let content = ref "discs;picks;moves\n";;

let picks_content = initializeHanoiTower number_picks number_discs;;

(* Generate the number of moves needed to resolve the towers of Hanoi
    and put them into a string in a csv style *)
for nbDiscs=3 to number_discs do
	for nbPick=3 to number_picks do
		let picks_content = initializeHanoiTower nbPick nbDiscs in
		moves_number := 0 ;
		hanoi nbDiscs picks_content nbDiscs 0 (nbPick - 1);
		content := !content^(string_of_int nbDiscs)^";"^(string_of_int nbPick)^";"^(string_of_int !moves_number)^"\n";
	done;
done;;

open Printf

let () =
(* Write message to file *)
let oc = open_out file in    (* create or truncate file, return channel *)
fprintf oc "%s\n" !content;   (* write something *)   
close_out oc;                (* flush and close the channel *)
