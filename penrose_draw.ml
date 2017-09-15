#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open_graph " 1280*720-0+0";;
open Printf

let floatDoublet double = match double with
	| (a,b) -> (int_of_float a, int_of_float b);;

let draw points color= 
	let pointsInt = Array.map floatDoublet points in
	set_color color;
	fill_poly pointsInt;;

(* The vertex which have an angle different from the two other vertex of
the triangle is put in the first place of the triangle*)

type triangle_type = Obtuse | Acute;;

let phi = (1. +. sqrt 5) /. 2.;;

(* Return the points that is at 1 / (1 + phi) ratio from the first point *)
let getDividingPoint (x1, y1) (x2, y2) =
    let x = x1 + (x1 - x2) * 1 /. (1 +. phi) in
    let y = y1 + (y1 - y2) * 1 /. (1 +. phi) in
    (x, y)
;;

let getTriangleColor triangle_type =
    match triangle_type with
        | Obtuse -> green
        | Acute -> red
;;

let rec divide generations points triangle_type =
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
        divide (generations - 1) acute_triangle Acute;
        divide (generations - 1) obtuse_triangle Obtuse
    end

and divideAcute generations points =
    (* Instead of cutting into three triangles, we cut into two triangles
        but in the end it will amount to the same thing *)
    let new_point1 = getDividingPoint points.(1) points.(0) in
    let obtuse_triangle = [| new_point; points.(0); points.(2) |] in
    let acute_triangle = [| points.(2); new_point; points.(1) |] in
    begin
        divide (generations - 1) acute_triangle Acute;
        divide generations obtuse_triangle Obtuse
    end
;;
