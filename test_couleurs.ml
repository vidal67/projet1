#load "graphics.cma";;
#load "unix.cma";;

Graphics.open_graph " 1280x720-0+0";;


let nombreGenerations = 5;;

let greyGradient n=
	Graphics.rgb (200*n/nombreGenerations) (200*n/nombreGenerations) (200*n/nombreGenerations);;

Graphics.set_color(Graphics.rgb 125 125 125);

for n=0 to 5 do
	Graphics.set_color(Graphics.rgb (200*n/5) (200*n/5) (200*n/5));
	Graphics.fill_poly [|(0,0);(10,50);(50,10)|];
	Unix.sleep 2;
done;

Unix.sleep 20;;
