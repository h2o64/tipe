(* Open image to analyse *)
let test_image = import_image "../Poincare_Index/fingerprint.jpg"

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

(* Get pixel luminance *)
let getLuminance cl =
		(* Relative luminance in colorimetric spaces
			Luminance (Standard for certain colour spaces): (0.2126*R + 0.7152*G + 0.0722*B)
			Luminance (Option 1): (0.299*R + 0.587*G + 0.114*B)
			Luminance (Option 2): sqrt( 0.299*R^2 + 0.587*G^2 + 0.114*B^2 )
			Source:
				* [1] Wikipédia - Relative luminance
				* [2] W3.org - https://www.w3.org/TR/AERT#color-contrast
				* [3] Darel Rex Finley - http://alienryderflex.com/hsp.html *)
			let luminance = 0.299 *. (float_of_int (cl.r) ** 2.)
									 +. 0.587 *. (float_of_int (cl.g) ** 2.)
									 +. 0.114 *. (float_of_int (cl.b) ** 2.) in
			sqrt luminance;;

(* Get pixel luminance *)
let getLuminance_from_int pix =
		(* Relative luminance in colorimetric spaces
			Luminance (Standard for certain colour spaces): (0.2126*R + 0.7152*G + 0.0722*B)
			Luminance (Option 1): (0.299*R + 0.587*G + 0.114*B)
			Luminance (Option 2): sqrt( 0.299*R^2 + 0.587*G^2 + 0.114*B^2 )
			Source:
				* [1] Wikipédia - Relative luminance
				* [2] W3.org - https://www.w3.org/TR/AERT#color-contrast
				* [3] Darel Rex Finley - http://alienryderflex.com/hsp.html *)
			let cl = rgbint_to_color pix in
			let luminance = 0.299 *. (float_of_int (cl.r) ** 2.)
									 +. 0.587 *. (float_of_int (cl.g) ** 2.)
									 +. 0.114 *. (float_of_int (cl.b) ** 2.) in
			sqrt luminance;;

(* Convert RGB to greyscale *)
let rgbToGreyScale color = (float_of_int (color.r + color.g + color.b))/.3.;;

(* Get all surrounding blocks *)
let getSurrounding i j image bloc_size =
	let ret = Array.make_matrix bloc_size bloc_size 0. in
	for k = 0 to 2 do
		for l = 0 to 2 do
			ret.(k).(l) <- getLuminance (rgbint_to_color (image.matrix.(i + (k - 1)).(j + (l-1))));
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

(* Actually, I don't need it ....

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
s
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
let pi = 4. *. atan 1.

(*
(* Apply a function to each element of a matrix *)
let matrixApply f (matrix : matrix) =
	for i = 0 to ((Array.length matrix) - 1) do
		for j = 0 to ((Array.length matrix.(0)) - 1) do
			matrix.(i).(j) <- f (matrix.(i).(j))
		done;
	done;;

(* Gaussian Smoothing *)


let gaussian_smoothin matrix =
	let cos_angles = matrix in
	let sin_angles = matrix in
	let cos_car x = (cos (x*.x)) in
	let sin_car x = (sin (x*.x)) in
	let ret = Array.make_matrix 3 3 0. in
	matrixApply cos_car cos_angles;
	matrixApply sin_car sin_angles;
	let cos_k = mult cos_angles gaussian_kernel in
	let sin_k = mult sin_angles gaussian_kernel in
	for i = 0 to 2 do
		for j = 0 to 2 do
			ret.(i).(j) <- ((atan2 sin_angles.(i).(j) cos_angles.(i).(j))/.2.)
		done;
	done;(ret : matrix);;
*)

(* Image Convolution *)
let convolve (kernel : matrix) (image_matrix : matrix) =
	let (h,w) = (Array.length image_matrix,Array.length image_matrix.(0)) in
	let ret = (Array.make_matrix h w 0. : matrix) in
	let r = ((Array.length kernel) - 1)/2 in (* Kernel is square *)
	let tmp = ref 0. in
	for img_i = 0 to (h - 1) do
		for img_j = 0 to (w - 1) do
				tmp := 0.;
				for kernel_i = -r to r do
					for kernel_j = -r to r do
						let x = img_i + kernel_i in
						let y = img_j + kernel_j in
						(* Exterior values are 0 *)
						if not ((x < 0) || (y < 0) || (x > (h - 1)) || (y > (w-1))) then
							tmp := !tmp +.
								(image_matrix.(x).(y)*.kernel.(kernel_i + r).(kernel_j + r))
					done;
				done;
				ret.(img_i).(img_j) <- (-1.) *. !tmp;
		done;
	done;
	ret;;

(* Uses Sobel operator and Gaussian filter *)
let hX = [|[|-1.;0.;1.|];[|-2.;0.;2.|];[|-1.;0.;1.|]|];;
let hY = [|[|-1.;-2.;-1.|];[|0.;0.;0.|];[|1.;2.;1.|]|];; (* Transposée de gX *)
let gaussian_kernel = [| (* Size = 5 *)
[|0.012841;0.026743;0.03415;0.026743;0.012841|];
[|0.026743;0.055697;0.071122;0.055697;0.026743|];
[|0.03415;0.071122;0.090818;0.071122;0.03415|];
[|0.026743;0.055697;0.071122;0.055697;0.026743|];
[|0.012841;0.026743;0.03415;0.026743;0.012841|];
|];;
let getAngles m =
	let f_m = convolve gaussian_kernel m in
	let gX = convolve hX f_m in
	let gY = convolve hY f_m in
	let ret = Array.make_matrix 3 3 0. in
	for i = 0 to 2 do
		for j = 0 to 2 do
			ret.(i).(j) <- ((atan2 gY.(i).(j) gX.(i).(j))*.180.)/.pi;
		done;
	done;(ret : matrix);;

(* Sum angles and get the sg type *)
let sumAngles i j (matrix : matrix) =
	let error = 20. in (* 10% of error *)
	let sum = ref 0. in
	let ret = {x = i ; y = j ; typ = 4} in
	for k = 0 to 2 do
		for l = 0 to 2 do
			if (k != 1) && (l != 1) then sum := !sum +. matrix.(k).(l)
		done;
	done;
	if (abs_float (!sum -. 180.)) < error then ret.typ<-(1)
	else if (abs_float (!sum +. 180.)) < error then ret.typ<-(2)
	else if (abs_float (!sum -. 360.)) < error then ret.typ<-(3);
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
	set_color red;
	for i = 0 to (image.height - 1) do
		for j = 0 to (image.width - 1) do
				if sps.(i).(j).typ < 4 then
					(moveto j i;
					draw_circle j i 3);
		done;
	done;
  let _ = read_key() in close_graph();;

(* List singularity points *)
let list_sp image =
	let sps = poincare_index image in
	for i = 0 to (image.height - 1) do
		for j = 0 to (image.width - 1) do
			if sps.(i).(j).typ < 4 then
				begin
					if sps.(i).(j).typ = 0 then print_string "Loop at " (* Loop *)
					else if sps.(i).(j).typ = 1 then print_string "Delta at " (* Delta *)
					else if sps.(i).(j).typ = 2 then print_string "Whorl at "; (* Whorl *)
					print_string (getFormat i j);
					print_string "\n";
				end;
		done;
	done;;

(* Get vector field 
let getVectorField image =
	let blocs = makeBlocList image 3 in
	let ret = Array.make_matrix (image.height) (image.width) {x = 0 ; y = 0 ; angle = 0} in
	for i = 0 to ((Array.length blocs) - 1) do
		let (x,y) = getCoordonates i image.width in
		ret.(x).(y) <- sumAngles x y (getAngles blocs.(i))
	done;
	ret;;
*)
