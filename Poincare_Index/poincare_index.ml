(* Open image to analyse *)
(* let test_image = import_image "../Poincare_Index/fingerprint.jpg" *)

(* Types *)
type bloc = Images.color array;;
type matrix = float array array;;
type sp = {mutable x : int ; mutable y : int ; mutable typ : int};; (* 0 = loop | 1 = delta | 2 = whorl | 3 = nothing*)

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
(* Get angles *)
let pi = 4. *. atan 1.

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

(* Get all the singularity points *)
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
