(* Open image to analyse *)
let test_image = import_image "../Orientation_Field/fingerprint.jpg"

(* Variable *)
let global_bloc_size = 3;;

(* Types *)
type bloc = Images.color array;;
type matrix = float array array;;

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
		let (h,w) = getBlocSize img bloc_size in
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
let hY = [|[|-1.;-2.;-1.|];[|0.;0.;0.|];[|1.;2.;1.|]|];; (* Transposé de gX *)
let getAngles m = 
	let gX = mult hX m in
	let gY = mult hY m in
	let ret = Array.make_matrix 3 3 0. in
	for i = 0 to 2 do
		for j = 0 to 2 do
			ret.(i).(j) <- atan2 gY.(i).(j) gX.(i).(j);
		done;
	done;(ret : matrix);;
	
	
	

	
