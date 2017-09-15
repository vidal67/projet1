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

draw [|(10.,40.);(200.,100.);(1000.,452.)|] green;;
Unix.sleep 4;;

let x = 2;;
print_int x;;
