(* Open image to analyse *)
let test_image = import_image "../Poincare_Index/ppf1.png"

(* Variable *)
let global_bloc_size = 3;;

(* Types *)
type bloc = Images.color array;;
type matrix = float array array;;
type sp = {mutable x : int ; mutable y : int ; mutable typ : int};; (* 0 = loop | 1 = delta | 2 = whorl | 3 = nothing*)

(* Get necessary bloc size *)
(* TO BE IMPROVED *)
let getBlocSize img bloc_size = (img.height - (img.height mod bloc_size), img.width - (img.width mod bloc_size));;

(* Get bloc number based on matrix coordonates *)
let getBlocCo i j h w bloc_size = (i mod h/bloc_size)*(w/bloc_size)+(j mod w/bloc_size);;

(* Get the pixel number inside the bloc *)
let getBlocNum i j bloc_size = bloc_size*(i mod (bloc_size)) + (j mod (bloc_size));;

(* Convert RGB to greyscale *)
let rgbToGreyScale color = (float_of_int (color.r + color.g + color.b))/.3.;;

(* Get all surrounding blocks *)
let getSurrounding i j image bloc_size =
	let ret = Array.make_matrix bloc_size bloc_size 0. in
	for k = 0 to 2 do
		for l = 0 to 2 do
			ret.(k).(l) <- rgbToGreyScale (rgbint_to_color (image.matrix.(i + (k - 1)).(j + (l-1))));
		done;
	done;(ret : matrix);;

(* Make matrix array for each bloc_size*bloc_size blocs *)
let makeBlocList img bloc_size =
		let (h,w) = (img.height,img.width) in
		let ret = Array.make (h*w)
						 ((Array.make_matrix bloc_size bloc_size 0. : matrix)) in
		for i = 1 to (h-2) do
			for j = 1 to (w-2) do
				ret.(i*w+j) <- getSurrounding i j img bloc_size;
			done;
		done;
		ret;;

(* Sanitizer - I'm checking if everything is okay *)
let sanitizer checkme bloc_size =
		let ret = ref true in
		for i = 0 to (Array.length checkme)-1 do
			ret := !ret && ((Array.length (checkme.(i) : matrix)) = bloc_size*bloc_size)
		done;
		!ret;;

(* Matrix multiplication *)
let mult (m0 : matrix) (m1 : matrix)= 
	let x0 = Array.length m0 and y0 = Array.length m0.(0) and
			x1 = Array.length m1 and y1 = Array.length m1.(0) in
	if y0 <> x1 then failwith "Incompatible matrices!"
		else
			let res_matrix = Array.make_matrix x0 y1 0. in
			for i = 0 to x0 - 1 do
				for j = 0 to y1 - 1 do
					for k = 0 to y0 - 1 do
						res_matrix.(i).(j) <- res_matrix.(i).(j) +. m0.(i).(k) *. m1.(k).(j)
					done
				done
			done;
	(res_matrix : matrix);;

(* Actually, I don't need it ....

(* Transpose a matrix *)
let transpose (m : matrix)=
	let n = Array.make_matrix (Array.length m.(0)) (Array.length m) 0. in
	for i = 0 to (Array.length m - 1) do
		let row_i = m.(i) in
			for j = 0 to (Array.length row_i - 1) do
				n.(j).(i) <- row_i.(j)
			done;
		done;
	(n : matrix);;

(* Determinant for 3x3 matrix *)
let det3x3 ([|[|a;d;g|];[|b;e;h|];[|c;f;i|]|] : matrix)=
			a *. (e*.i -. h*.f)
		-. d *. (b*.i -. h*.c)
		+. g *. (b*.f -. e*.c);;

(* Get the transco-matrix *)
let getTransco3x3 (m : matrix) =
	let mT = transpose m in
	let ret = Array.make_matrix 3 3 0. in
	match mT with [|[|a;d;g|];[|b;e;h|];[|c;f;i|]|] ->
		ret.(0).(0) <- (e*.i -. f*.h);
		ret.(0).(1) <- (-1.) *. (b*.i -. h*.c);
		ret.(0).(2) <- (b*.f -. c*.e);
		ret.(1).(0) <- (-1.) *. (d*.i -. g*.f);
		ret.(1).(1) <- (a*.i -. g*.c);
		ret.(1).(2) <- (-1.) *. (a*.f -. d*.c);
		ret.(2).(0) <- (d*.h -. g*.e);
		ret.(2).(1) <- (-1.) *. (a*.h -. b*.g);
		ret.(2).(2) <- (a*.e -. b*.d);
	(ret : matrix);;

(* Get the reverse of a 3x3 matrix *)
let getReverse3x3 (m : matrix) = 
	let detm = det3x3 m in
	let transco = getTransco3x3 m in
	let ret = Array.make_matrix 3 3 0. in
	if (detm = 0.) then failwith "Error det(M) = 0";
	for i = 0 to 2 do
		for j = 0 to 2 do
			ret.(i).(j) <- (1./.(detm))*.transco.(i).(j)
		done;
	done;(ret : matrix);;
*)

(* Get angles *)
(* Uses Sobel operator *)
let hX = [|[|-1.;0.;1.|];[|-2.;0.;2.|];[|-1.;0.;1.|]|];;
let hY = [|[|-1.;-2.;-1.|];[|0.;0.;0.|];[|1.;2.;1.|]|];; (* Transposée de gX *)
let getAngles m = 
	let gX = mult hX m in
	let gY = mult hY m in
	let ret = Array.make_matrix 3 3 0. in
	for i = 0 to 2 do
		for j = 0 to 2 do
			ret.(i).(j) <- atan2 gY.(i).(j) gX.(i).(j);
		done;
	done;(ret : matrix);;

(* Sum angles and get the sg type *)
let sumAngles i j (matrix : matrix) =
	let pi = 4. *. atan 1. in
	let error = (0.001/.100.)*.pi in (* 1% of error *)
	let sum = ref 0. in
	let ret = {x = i ; y = j ; typ = 4} in
	for i = 0 to 2 do
		for j = 0 to 2 do
			if i != j then sum := !sum +. matrix.(i).(j)
		done;
	done;
	if (abs_float (!sum -. pi)) < error then ret.typ<-(1)
	else if (abs_float (!sum +. pi)) < error then ret.typ<-(2)
	else if (abs_float (!sum -. 2.*.pi)) < error then ret.typ<-(3);
	ret;;

(* Get coordonates from array position *)
let getCoordonates ind w = ((ind/w),(ind mod w));;

(* Get all the singularity points  *)
let poincare_index image =
	let blocs = makeBlocList image 3 in
	let ret = Array.make_matrix (image.height) (image.width) {x = 0 ; y = 0 ; typ = 4} in
	for i = 0 to ((Array.length blocs) - 1) do
		let (x,y) = getCoordonates i image.width in
		ret.(x).(y) <- sumAngles x y (getAngles blocs.(i))
	done;
	ret;;

(* Get the right image format *)
let getFormat height width =
	let s_height = string_of_int height in
	let s_width = string_of_int width in
	String.concat "" [" ";s_height;"x";s_width];;

(* Display singularity points *)
let display_sp image =
	let sps = poincare_index image in
	open_graph (getFormat image.width image.height);
  draw_image (make_image image.matrix) 0 0;
	for i = 0 to (image.height - 1) do
		for j = 0 to (image.width - 1) do
			if sps.(i).(j).typ < 4 then
				begin
					if sps.(i).(j).typ = 0 then set_color red
					else if sps.(i).(j).typ = 1 then set_color blue
					else if sps.(i).(j).typ = 2 then set_color green;
					draw_circle j i 2 (* /!\ *)
				end;
		done;
	done;
  let _ = read_key() in close_graph();;
