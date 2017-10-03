#load "graphics.cma";;
#load "unix.cma";;

let number_generations = 7;;
let time_between_draw = 0.1;;
let obtuse_triangle_color = Graphics.black;;
let acute_triangle_color = Graphics.white;;

let greyGradient n =
    let grey_value = 220 * n / number_generations in
	Graphics.rgb grey_value grey_value grey_value
;;

let floatDoublet double = match double with
	| (a,b) -> (int_of_float a, int_of_float b);;

let getReverseArray a =
    let a_length = Array.length a in
    if a_length = 0 then
        [||]
    else
    begin
        (* We have to initialize the array with something, so we take
            `a.(0)` *)
        let reversed_array = Array.make a_length a.(0) in
        for i = 0 to a_length - 1 do
            reversed_array.(i) <- a.((a_length - 1) - i)
        done;
        reversed_array
    end
;;

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
    drawFilledPolygon points color;
    drawHollowPolygon points Graphics.black;
    Unix.sleepf time_between_draw
;;

let add_points point1 point2 =
    let x1, y1 = point1 in
    let x2, y2 = point2 in
    (x1 +. x2, y1 +. y2)
;;

(* The vertex which have an angle different from the two other vertices of
    the triangle is put in the first place of the triangle *)

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
        | Obtuse -> obtuse_triangle_color
        | Acute -> acute_triangle_color
;;

let rec divideSimple generations points triangle_type =
    (* Divide a golden triangle so that the last generations of triangles
        are of the same size *)
    if generations <= 0 then
        draw points (getTriangleColor triangle_type)
    else
        match triangle_type with
            | Obtuse -> divideObtuse generations points
            | Acute -> divideAcute generations points

and divideObtuse generations points =
    let new_point = getDividingPoint points.(1) points.(2) in
    let acute_triangle = [| points.(2); new_point; points.(0) |] in
    let obtuse_triangle = [| new_point; points.(0); points.(1) |] in
    begin
        divideSimple (generations - 1) acute_triangle Acute ;
        divideSimple (generations - 1) obtuse_triangle Obtuse
    end

and divideAcute generations points =
    (* Instead of cutting into three triangles, we cut into two triangles
        but in the end it will amount to the same thing *)
    let new_point = getDividingPoint points.(1) points.(0) in
    let obtuse_triangle = [| new_point; points.(0); points.(2) |] in
    let acute_triangle = [| points.(2); new_point; points.(1) |] in
    begin
        divideSimple (generations - 1) acute_triangle Acute;
        divideSimple generations obtuse_triangle Obtuse;
    end
;;

let divideFancy generations points triangle_type =
    (* Divide a golden triangle so that the resulting tiling is pleasant to
        look at, it will also display the triangle that is currently being
        divided, the triangle is displayed with a grescale that get darker
        with incresing generations.

        Inside this function, the convention on the order of the vertices
        of the triangles do not hold, it is easier for us to use the 
        reverse order *)

    let rec divide generations points triangle_type =
        if generations <= 0 then
            draw points (getTriangleColor triangle_type)
        else
            match triangle_type with
                | Obtuse -> divideObtuse generations points
                | Acute -> divideAcute generations points

    and divideObtuse generations points =
        (* To have a better tiling we chooses carefully where we divide the
            triangle and in which order we pass the points to the next divide
            procedure, follwing an idea from
            http://images.math.cnrs.fr/Un-parquet-de-Penrose.html?lang=fr#nb3 *)
        let new_point1 = getDividingPoint points.(0) points.(2) in
        let new_point2 = getDividingPoint points.(1) points.(2) in
        let acute_triangle = [| points.(1); new_point2; new_point1 |] in
        let obtuse_triangle1 = [| points.(1); new_point1; points.(0) |] in
        let obtuse_triangle2 = [| points.(2); new_point2; new_point1 |] in
        begin
            let couleur = greyGradient generations in
            draw points couleur;
            divide (generations - 1) acute_triangle Acute ;
            divide (generations - 1) obtuse_triangle1 Obtuse;
            divide (generations - 1) obtuse_triangle2 Obtuse
        end

    and divideAcute generations points =
        (* This is the same idea as `divideAcute` *)
        let new_point = getDividingPoint points.(1) points.(2) in
        let acute_triangle = [|points.(1);  new_point; points.(0)|] in
        let obtuse_triangle = [| points.(2); new_point; points.(0) |] in
        begin
            let couleur = greyGradient generations in
            draw points couleur;
            divide (generations - 1) acute_triangle Acute;
            divide (generations - 1) obtuse_triangle Obtuse;
        end
    in
    let reversed_points = getReverseArray points in 
    divide generations reversed_points triangle_type
;;

let getAcuteTriangle size position =
    (* size is the size of the side with unit length *)
    let height = sqrt (phi *. phi -. 0.25) in
    let shape = [| (size /. 2., height *. size); (size, 0.); (0., 0.) |] in
    Array.map (add_points position) shape 
;;

Graphics.open_graph " 1280x720-0+0";;
divideFancy number_generations (getAcuteTriangle 300. (100., 100.)) Acute;;
Unix.sleep 10;;
