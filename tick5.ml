PolyML.Compiler.debug := true;
open PolyML.Debug;
trace true;

type color = int*int*int  (* RGB colour components, 0..255 *)
type xy = int*int       (* points (x, y) and sizes (w, h) *)
datatype image = Image of xy * color array array;

fun image ((w, h):xy) (r,g,b) = Image((w,h), Array.tabulate(h, fn i => Array.tabulate(w, fn i => (r,g,b))));

fun size (Image((w,h), x)) = (w,h);
fun imArray (Image((w,h), x)) = x;
fun imArray2 (Image((w,h), x)) i = Array.sub(x, i);
fun drawPixel (Image((w,h), xs)) (r,g,b) (x,y) = Array.update(Array.sub(xs, y), x, (r,g,b));

fun format4 i = StringCvt.padLeft #" " 4 (Int.toString i);


fun colorToString color =
	let val (r,g,b) = color
	in format4 r ^ format4 g ^  format4 b
	end;

fun toPPM image filename =
	let val oc = TextIO.openOut filename
		val Image((w,h) , arr) = image

	in
		TextIO.output(oc,"P3\n" ^ Int.toString w ^ " " ^ Int.toString h ^ "\n255\n");
		(* code to output image rows, one per line goes here *)
		Array.app (fn i  => (Array.app (fn j => (TextIO.output(oc, colorToString j))) i; TextIO.output(oc,"\n"))) arr;
		TextIO.closeOut oc
	end;

	


fun drawHoriz image (r,g,b) (x,y) i = if i >0 then ((drawPixel image (r,g,b) (x,y)) ; (drawHoriz image (r,g,b) (x+1,y) (i-1)) ) else ();
fun drawVert image (r,g,b) (x,y) i = if i >0 then ((drawPixel image (r,g,b) (x,y)) ; (drawVert image (r,g,b) (x,y+1) (i-1)) ) else ();
fun drawDiag image (r,g,b) (x,y) i = if i >0 then ((drawPixel image (r,g,b) (x,y)) ; (drawDiag image (r,g,b) (x+1,y+1) (i-1)) ) else ();

(*drawDiag im (255, 255, 255) (0,0) 2;
toPPM im "testimage2.ppm";*)

fun drawLine (Image((w,h),arr)) (r,g,b) (u1,v1) (u2,v2) = 
	let val dx = Int.abs(u2 - u1)
		val dy = Int.abs(v2 - v1)
		val sx = if u1 < u2 then 1 else ~1
		val sy = if v1 < v2 then 1 else ~1
		val err = dx - dy
			fun drawLineLoop (x1,y1) (x2,y2) err = 
				let val e2 = 2*err

				in (drawPixel (Image((w,h),arr)) (r,g,b) (x1, y1) ; if x1 = x2 andalso y1 = y2 then Image((w,h),arr) else
					 
					if e2 > ~dy andalso e2 < dx then 
						
						(drawLineLoop (x1 + sx, y1 + sy) (x2, y2) (err - dy + dx))
					else
						if e2 > ~dy then drawLineLoop (x1 + sx, y1 ) (x2, y2) (err - dy) else 
							if e2 < dx then drawLineLoop (x1 , y1 + sy) (x2, y2) (err + dx) else Image((w,h),arr))

				end;

	in drawLineLoop (u1,v1) (u2,v2) err

	end;

val im=image (5,5) (0,0,0); (*drawLine im (2,2,2) (0,0) (4,4);*) drawLine im (1,1,1) (4,0) (0,4);
toPPM im "testimage.ppm";
