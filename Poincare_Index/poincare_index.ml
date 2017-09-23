(* Open image to analyse *)
let test_image = import_image "../Poincare_Index/fingerprint.jpg"

(* Types *)
(* 0 = loop | 1 = delta | 2 = whorl | 3 = nothing *)
type sp = {mutable x : int ; mutable y : int ; mutable typ : int};;

(* Image Convolution - Gaussian filter *)
let (gaussian_kernel : float matrix) = [| (* Size = 5 *)
[|0.012841;0.026743;0.03415;0.026743;0.012841|];
[|0.026743;0.055697;0.071122;0.055697;0.026743|];
[|0.03415;0.071122;0.090818;0.071122;0.03415|];
[|0.026743;0.055697;0.071122;0.055697;0.026743|];
[|0.012841;0.026743;0.03415;0.026743;0.012841|];
|];;

(* Do convolution on only one pixel *)
let convolve i j (kernel : 'a matrix) (image_matrix : 'a matrix) r =
	let tmp = ref 0. in
	let (h,w) = ((Array.length image_matrix),(Array.length image_matrix.(0))) in
	for m = 0 to (r - 1) do
		for n = 0 to (r - 1) do
			(* Use zero-padding to extend the image *)
			if not(((i-m) < 0) || ((j-n) < 0) || ((i-m) > (h-1)) || ((j-n) > (w-1))) then 
				tmp := !tmp +.
					(kernel.(m).(n)*.image_matrix.(i - m).(j - n))
		done;
	done;
	!tmp;;

(* Convolve whole matrix *)
let convolve_matrix (kernel : 'a matrix) (m : 'a matrix) =
		let r = Array.length kernel in (* Kernel is square *)
		let (h,w) = ((Array.length m),(Array.length m.(0))) in
		let ret = Array.make_matrix h w 0. in
		for i = 0 to (h - 1) do
			for j = 0 to (w - 1) do
				ret.(i).(j) <- (convolve i j kernel m r)
			done;
		done;
		ret;;

(* Apply Gaussian filter on image *)
let applyGaussianFilter matrix = convolve_matrix gaussian_kernel matrix;;

(* Uses Sobel operator *)
(* https://en.wikipedia.org/wiki/Prewitt_operator *)
(* https://en.wikipedia.org/wiki/Sobel_operator *)
let pi = 4. *. atan 1.
(* let hX = [|[|1.;0.;-1.|];[|2.;0.;-2.|];[|1.;0.;-1.|]|];; (* Sobel *)
let hY = [|[|1.;2.;1.|];[|0.;0.;0.|];[|-1.;-2.;-1.|]|];; (* Transposée de gX *) *)
let hX = [|[|1.;0.;-1.|];[|1.;0.;-1.|];[|1.;0.;-1.|]|];; (* Prewitt *)
let hY = [|[|1.;1.;1.|];[|0.;0.;0.|];[|-1.;-1.;-1.|]|];; (* Transposée de gX *)
let getAngles m =
	let (h,w) = ((Array.length m),(Array.length m.(0))) in
	let gX = convolve_matrix hX m in
	let gY = convolve_matrix hY m in
	let ret = Array.make_matrix h w 0. in
	for i = 0 to (h-1) do
		for j = 0 to (w-1) do
			ret.(i).(j) <- (atan2 gY.(i).(j) gX.(i).(j))
		done;
	done;(ret : float matrix);;

(* Sum angles and get the sg type *)
let sumAngles i j (matrix : float matrix) =
	let error = (5./.100.)*.pi in (* 5% of error *)
	let sum = ref 0. in
	let ret = {x = i ; y = j ; typ = 3} in
	for k = 0 to 2 do
		for l = 0 to 2 do
			if (k != 1) && (l != 1) then sum := !sum +. matrix.(k).(l)
		done;
	done;
	if (abs_float (!sum -. pi)) < error then ret.typ<-(0)
	else if (abs_float (!sum +. pi)) < error then ret.typ<-(1)
	else if (abs_float (!sum -. 2.*.pi)) < error then ret.typ<-(2);
	ret;;

(* Get coordonates from array position *)
let getCoordonates ind w = ((ind/w),(ind mod w));;

(* Get all the singularity points *)
let poincare_index image =
	(* let blocs = makeBlocList (getAngles (applyGaussianFilter image.matrix)) 3 in *)
	let blocs = makeBlocList (getAngles image.matrix) 3 in
	let ret = Array.make_matrix (image.height) (image.width) {x = 0 ; y = 0 ; typ = 3} in
	let is_border x y = (x = 0) || (x = (image.height - 1)) || (y = 0) || (y = (image.width - 1)) in
	for i = 0 to ((Array.length blocs) - 1) do
		let (x,y) = (blocs.(i).x,blocs.(i).y) in
		ret.(x).(y) <- (if is_border x y then
											{x = x ; y = y ; typ = 3}
									 else 
											sumAngles x y (getAngles blocs.(i).matrix))
	done;
	ret;;

(* Display singularity points *)
let display_sp image =
	let sps = poincare_index (imageToGreyScale image) in
	open_graph (getFormat image.width image.height);
	draw_image (make_image image.matrix) 0 0;
	set_color red;
	for i = 0 to (image.height - 1) do
		for j = 5 to (image.width - 1) do
				if sps.(i).(j).typ < 3 then
					(moveto j i;
					draw_circle j i 2); (* WARNING: WTF *)
		done;
	done;
	let _ = read_key() in close_graph();;

(* List singularity points *)
let list_sp image =
	let sps = poincare_index (imageToGreyScale image) in
	for i = 0 to (image.height - 1) do (* 5 = kernel width *)
		for j = 5 to (image.width - 1) do
			if sps.(i).(j).typ < 3 then
				begin
					if sps.(i).(j).typ = 0 then print_string "Loop at" (* Loop *)
					else if sps.(i).(j).typ = 1 then print_string "Delta at" (* Delta *)
					else if sps.(i).(j).typ = 2 then print_string "Whorl at"; (* Whorl *)
					print_string (getFormat i j);
					print_string "\n";
				end;
		done;
	done;;

(* Testing *)
let gaussian_test image =
	let bw_img = imageToGreyScale image in
	let m = applyGaussianFilter bw_img.matrix in
	let last = matrixApply rgb_of_greyscale m in
	dessiner_image last;;
	
