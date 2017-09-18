#load "graphics.cma";;
#load "unix.cma";;

let floatDoublet double = match double with
	| (a,b) -> (int_of_float a, int_of_float b);;

let drawLine point1 point2 =
    (* Draw a line between point1 and point2 *)
    let x1, y1 = floatDoublet point1 in
    let x2, y2 = floatDoublet point2 in
    Graphics.moveto x1 y1;
    Graphics.lineto x2 y2;
;;

let drawHollowPolygon points color = 
    (* Draw in the given color the edges of the polygons represented by
        points *)
    let points_length = Array.length points in
    Graphics.set_color color;
    for i=0 to points_length - 1 do
        drawLine points.(i) points.((i + 1) mod points_length)
    done
;;

let drawFilledPolygon points color = 
    (* Draw in the given color the polygons represented by points *)
	let pointsInt = Array.map floatDoublet points in
	Graphics.set_color color;
	Graphics.fill_poly pointsInt
;;

let draw points color = 
    (* We draw the polygon then its edges so that the edges are apparent
        on the screen *)
    (* drawFilledPolygon points color; *)
    drawHollowPolygon points Graphics.black
;;

let add_points point1 point2 =
    let x1, y1 = point1 in
    let x2, y2 = point2 in
    (x1 +. x2, y1 +. y2)
;;

(* The vertex which have an angle different from the two other vertex of
    the triangle is put in the first place of the triangle*)

type triangle_type = Obtuse | Acute;;

let phi = (1. +. sqrt 5.) /. 2.;;

(* Return the points that is at 1 / (1 + phi) ratio from the first point *)
let getDividingPoint (x1, y1) (x2, y2) =
    let x = x1 +. (x2 -. x1) *. 1. /. (1. +. phi) in
    let y = y1 +. (y2 -. y1) *. 1. /. (1. +. phi) in
    (x, y)
;;

let getTriangleColor triangle_type =
    match triangle_type with
        | Obtuse -> Graphics.black
        | Acute -> Graphics.yellow
;;

let rec divide generations points triangle_type =
    if generations <= 0 then
    begin
		Unix.sleep 1;
        draw points (getTriangleColor triangle_type);
    end
    else
        match triangle_type with
            | Obtuse -> divideObtuse generations points
            | Acute -> divideAcute generations points

and divideObtuse generations points =
    let new_point = getDividingPoint points.(1) points.(2) in
    let acute_triangle = [| points.(2); new_point; points.(0) |] in
    let obtuse_triangle = [| new_point; points.(0); points.(1) |] in
    begin
        divide (generations - 1) acute_triangle Acute ;
        divide (generations - 1) obtuse_triangle Obtuse
    end

and divideAcute generations points =
    (* Instead of cutting into three triangles, we cut into two triangles
        but in the end it will amount to the same thing *)
    let new_point = getDividingPoint points.(1) points.(0) in
    let obtuse_triangle = [| new_point; points.(0); points.(2) |] in
    let acute_triangle = [| points.(2); new_point; points.(1) |] in
    begin
        divide (generations - 1) acute_triangle Acute;
        divide generations obtuse_triangle Obtuse;
    end
;;

let getAcuteTriangle size position =
    (* size is the size of the side with unit length *)
    let height = sqrt (phi *. phi -. 0.25) in
    let shape = [| (size /. 2., height *. size); (size, 0.); (0., 0.) |] in
    Array.map (add_points position) shape
;;

Graphics.open_graph " 1280x720-0+0";;
divide 4 (getAcuteTriangle 300. (100., 100.)) Acute;;
Unix.sleep 2;;
