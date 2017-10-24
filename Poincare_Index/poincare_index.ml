(* Open image to analyse *)
let test_image = import_image "../Poincare_Index/fingerprint.jpg"

(* Types *)
(* 0 = loop | 1 = delta | 2 = whorl | 3 = nothing *)
type sp = {mutable x : int ; mutable y : int ; mutable typ : int};;

(* Image Convolution - Kernel Collection *)
let (gaussian_kernel : float matrix) = [| (* Size = 5 *)
[|0.012841;0.026743;0.03415;0.026743;0.012841|];
[|0.026743;0.055697;0.071122;0.055697;0.026743|];
[|0.03415;0.071122;0.090818;0.071122;0.03415|];
[|0.026743;0.055697;0.071122;0.055697;0.026743|];
[|0.012841;0.026743;0.03415;0.026743;0.012841|];
|];;
let (sharpen_kernel : float matrix) = [|
[|0.;-1.;0.|];
[|-1.;5.;-1.|];
[|0.;5.;-1.|];
|];;
let (edge1_kernel : float matrix) = [|
[|1.;0.;-1.|];
[|0.;0.;0.|];
[|-1.;0.;1.|];
|];;
let (edge2_kernel : float matrix) = [|
[|0.;1.;0.|];
[|1.;-4.;1.|];
[|0.;1.;0.|];
|];;
let (edge3_kernel : float matrix) = [|
[|-1.;-1.;-1.|];
[|-1.;8.;-1.|];
[|-1.;-1.;-1.|];
|];;
let (unsharp_masking_kernel : float matrix) = [| (* Size = 5 *)
[|0.012841;0.026743;0.03415;0.026743;0.012841|];
[|0.026743;0.055697;0.071122;0.055697;0.026743|];
[|0.03415;0.071122;1.859375;0.071122;0.03415|];
[|0.026743;0.055697;0.071122;0.055697;0.026743|];
[|0.012841;0.026743;0.03415;0.026743;0.012841|];
|];;

(* Do convolution on only one pixel *)
let convolve i j kernel image_matrix =
	let tmp = ref 0. in
	let r = Array.length kernel in (* Kernel is square *)
	let (h,w) = ((Array.length image_matrix),(Array.length image_matrix.(0))) in
	for m = 0 to (r - 1) do
		for n = 0 to (r - 1) do
			(* Use zero-padding to extend the image *)
			let (a,b) = ((i + m - (r/2)),(j + n - (r/2))) in
			if not((a < 0) || (b < 0) || (a > (h-1)) || (b > (w-1))) then 
				tmp := !tmp +.(kernel.(m).(n)*.image_matrix.(a).(b))
		done;
	done;
	!tmp;;

(* Convolve whole matrix *)
let convolve_matrix kernel m =
		let (h,w) = ((Array.length m),(Array.length m.(0))) in
		let ret = Array.make_matrix h w 0. in
		for i = 0 to (h - 1) do
			for j = 0 to (w - 1) do
				ret.(i).(j) <- (convolve i j kernel m)
			done;
		done;
		ret;;

(* Apply filter on an image *)
let applyFilter matrix kernel = convolve_matrix kernel matrix;;

(* Uses Sobel operator *)
(* https://en.wikipedia.org/wiki/Prewitt_operator *)
(* https://en.wikipedia.org/wiki/Sobel_operator *)
let pi = 4. *. atan 1.
let (hX : float matrix) = [|[|1.;0.;-1.|];[|2.;0.;-2.|];[|1.;0.;-1.|]|];; (* Sobel *)
let (hY : float matrix) = [|[|1.;2.;1.|];[|0.;0.;0.|];[|-1.;-2.;-1.|]|];; (* Transposée de gX *)
(* Based of Kass and Witkin (1987) researches *)
let getAngles m bloc_size =
	let (h,w) = ((Array.length m),(Array.length m.(0))) in
	let (h_new,w_new) = ((h-(h mod bloc_size))/bloc_size,(w-(w mod bloc_size))/bloc_size) in
	let ret = Array.make_matrix h_new w_new 0. in
	for i = 0 to (h_new-1) do
		for j = 0 to (w_new-1) do
			let (global_x,global_y) = ((i/bloc_size)+(bloc_size/2),(j/bloc_size)+(bloc_size/2)) in
			let num = ref 0. in
			let dem = ref 0. in
			for h = -bloc_size/2 to bloc_size/2 do
				for k = -bloc_size/2 to bloc_size/2 do
					let tmp_x = convolve (global_x + h) (global_y + k) hX m in
					let tmp_y = convolve (global_x + h) (global_y + k) hY m in
					(num := !num +. (2. *. tmp_x *. tmp_y);
					dem := !dem +. (tmp_x**2. -. tmp_y**2.));
				done;
			done;
			ret.(i).(j) <- (pi +. (atan2 !num !dem))/.2.;
		done;
	done;ret;;

(* Get the angle between two angles *)
let getAngleBetween x y = pi -. abs_float (abs_float ((x -. y) -. pi));

open Printf
let print_array a =
	Array.iter (printf "%d ") a;
	print_string "\n";;

let print_matrix m =
	for i = 0 to ((Array.length m)-1) do
		print_array m.(i)
	done;;

(* Make a array from a matrix *)
let array_of_matrix m =
	let (h,w) = ((Array.length m),(Array.length m.(0))) in
	let liste = Array.make (h*w-1) 0. in
	let count = ref 0 in
	for k = 0 to (h-1) do
		for l = 0 to (w-1) do
			(* Ignore (1,1) *)
			if not((k,l) = (1,1)) then
				(liste.(!count)<-m.(k).(l);
				count := (!count + 1);)
		done;
	done;liste;;

(* Percentage of pi from tolerance *)
let allowance tolerance = ((float_of_int tolerance)/.100.)*.pi;;

(* Sum angles and get the sg type *)
let sumAngles i j matrix tolerance =
	let ret = {x = i ; y = j ; typ = 3} in
	let liste = array_of_matrix matrix in
	let error = allowance tolerance in
	let sum = ref liste.(0) in
	for cur = 1 to 7 do
		let next = ((cur+1) mod 8) in
		(if (abs_float (getAngleBetween liste.(cur) liste.(cur-1))) > pi/.2. then
			liste.(cur)<-liste.(cur)+.pi;);
		sum := !sum +. getAngleBetween liste.(cur) liste.(next)
	done;
	print_string "\n sum = ";
	print_float !sum;
	if ((pi -. error) <= !sum) && (!sum <= (pi +. error)) then ret.typ<-(0);
	if (((-1.)*.pi -. error) <= !sum) && (!sum <= ((-1.)*.pi +. error)) then ret.typ<-(1);
	if ((2.*.pi -. error) <= !sum) && (!sum <= (2.*.pi +. error)) then ret.typ<-(2);
	ret;;

(* Get all the singularity points *)
let poincare_index matrix bloc_size tolerance =
	let (h,w) = ((Array.length matrix),(Array.length matrix.(0))) in
	let blocs = makeBlocList (getAngles matrix bloc_size) 3 in
	let ret = Array.make_matrix h w {x = 0 ; y = 0 ; typ = 3} in
	for i = 0 to ((Array.length blocs) - 1) do
		let (x,y) = (blocs.(i).x,blocs.(i).y) in
		ret.(x).(y) <-sumAngles x y blocs.(i).matrix tolerance
	done;
	ret;;

(* Get circle location *)
let getCircleLocation i j h bloc_size =
	let (x,y) = ((i*bloc_size+(bloc_size/2)),(j*bloc_size+(bloc_size/2))) in
	((y),(h - x));;

(* Get line coordonates *)
let getStartEndLine x y teta bloc_size =
	(* Descrete values : multiples of 45° *)
	let deg_of_rad x = int_of_float ((x*.180.)/.pi) in
	let deg = (abs (deg_of_rad teta))/45 in
	let w = bloc_size/2 in
	print_string "\ndeg = ";
	print_int (deg*45);
	if ((deg mod 4) = 0) then (x,y+w,x,y-w)
	else if ((deg mod 4) = 1) then (x+w,y+w,x-w,y-w)
	else if ((deg mod 4) = 2) then (x-w,y,x+w,y)
	else if ((deg mod 4) = 3) then (x-w,y+w,x+w,y-w)
	else (0,0,0,0);;

(* Display vector field *)
let vector_field image bloc_size =
	let grey_im = imageToGreyScale image in
	let angles = getAngles grey_im.matrix bloc_size in
	set_line_width 2;
	set_color red;
	draw_image (make_image image.matrix) 0 0;
	for i = 0 to ((Array.length angles)-1) do
		for j = 0 to ((Array.length angles.(0))-1) do
			let (x,y) = getCircleLocation i j image.height bloc_size in
			let (x1,y1,x2,y2) = (getStartEndLine x y angles.(i).(j) bloc_size) in
			draw_segments [|x1,y1,x2,x2|];
		done;
	done;;

(* Display singularity points *)
let display_sp image bloc_size tolerance =
	let grey_im = imageToGreyScale image in
	let sps = poincare_index grey_im.matrix bloc_size tolerance in
	(* open_graph (getFormat image.width image.height); *)
	set_line_width 4;
	draw_image (make_image image.matrix) 0 0;
	for i = 0 to ((Array.length sps) - 1) do
		for j = 0 to ((Array.length sps.(0)) - 1) do
				if sps.(i).(j).typ < 3 then
				begin
					if sps.(i).(j).typ = 0 then set_color red; (* Loop *)
					if sps.(i).(j).typ = 1 then set_color green; (* Delta *)
					if sps.(i).(j).typ = 2 then set_color blue; (* Whorl *)
					let (x,y) = getCircleLocation i j image.height bloc_size in
					moveto x y;
					draw_circle x y (bloc_size/2)
				end;
		done;
	done;;

(* Testing *)
let filter_test image kernel =
	let bw_img = imageToGreyScale image in
	let m = applyFilter bw_img.matrix kernel in
	let last = matrixApply rgb_of_greyscale m in
	open_graph (getFormat image.width image.height);
	dessiner_image last;;

let blockedToFull h w bloc_size matrix =
	let ret = Array.make_matrix h w 255. in
	for i = 0 to ((h - 1) - (h mod bloc_size)) do
		for j = 0 to ((w - 1) - (w mod bloc_size)) do
			ret.(j).(j) <- matrix.(i/bloc_size).(j/bloc_size)
		done;
	done;ret;;

let displayAnyMatrix matrix =
	let (h,w) = ((Array.length matrix),(Array.length matrix.(0))) in
	let last = matrixApply rgb_of_greyscale matrix in
	open_graph (getFormat w h);
	dessiner_image last;;
