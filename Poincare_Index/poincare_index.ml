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
let convolve i j (kernel : 'a matrix) (image_matrix : 'a matrix) r =
	let tmp = ref 0. in
	let (h,w) = ((Array.length image_matrix),(Array.length image_matrix.(0))) in
	for m = 0 to (r - 1) do
		for n = 0 to (r - 1) do
			(* Use zero-padding to extend the image *)
			let (a,b) = ((i + m - (r/2)),(j + n - (r/2))) in
			if not((a < 0) || (b < 0) || (a > (h-1)) || (b > (w-1))) then 
				tmp := !tmp +.
					(kernel.(m).(n)*.image_matrix.(a).(b))
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
		(ret : 'a matrix);;

(* Apply filter on an image *)
let applyFilter (matrix : 'a matrix) (kernel : 'a matrix) =
			(convolve_matrix kernel matrix : 'a matrix);;

(* Uses Sobel operator *)
(* https://en.wikipedia.org/wiki/Prewitt_operator *)
(* https://en.wikipedia.org/wiki/Sobel_operator *)
let pi = 4. *. atan 1.
let (hX : float matrix) = [|[|1.;0.;-1.|];[|2.;0.;-2.|];[|1.;0.;-1.|]|];; (* Sobel *)
let (hY : float matrix) = [|[|1.;2.;1.|];[|0.;0.;0.|];[|-1.;-2.;-1.|]|];; (* Transposée de gX *)
(* let (hX : float matrix) = [|[|1.;0.;-1.|];[|1.;0.;-1.|];[|1.;0.;-1.|]|];; (* Prewitt *)
let (hY : float matrix) = [|[|1.;1.;1.|];[|0.;0.;0.|];[|-1.;-1.;-1.|]|];; (* Transposée de gX *) *)
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

(* Get the angle between two angles *)
let getAngleBetween x y =
	let ret = ref (x-y) in
	let signum p = if (p > 0) then (-1) else 1 in
	if ((abs !ret) > 180) then
		ret := (-1) * (signum !ret) * (360 - (abs !ret));
	!ret;;

(* Sum angles and get the sg type *)
let sumAngles i j (matrix : float matrix) =
	let error = 3 in (* 3° of error *)
	let ret = {x = i ; y = j ; typ = 3} in
	let deg_of_rad x = int_of_float ((x*.180.)/.pi) in
	let liste = Array.make 8 0 in
	let count = ref 0 in
	for k = 0 to 2 do
		for l = 0 to 2 do
			if ((k != 1) && (l != 1)) then
				(liste.(!count)<-(deg_of_rad matrix.(k).(l));
				count := (!count + 1))
		done;
	done;
	let sum = ref 0 in
	for m = 0 to 7 do
		(* if abs (getAngleBetween liste.(m) liste.((m+1) mod 8)) > 90 then
			(liste.((m+1) mod 8)<-((liste.((m+1) mod 8)) + 180)); *)
		sum := !sum + (getAngleBetween liste.(m) liste.((m+1) mod 8))
	done;
	if (abs (!sum - 180)) < error then ret.typ<-(0)
	else if (abs (!sum + 180)) < error then ret.typ<-(1)
	else if (abs (!sum - 360)) < error then ret.typ<-(2);
	ret;;

(* Get all the singularity points *)
let poincare_index image =
	let blocs =
		makeBlocList (getAngles (applyFilter image.matrix unsharp_masking_kernel)) 3 in (* WARNING: WTF *)
	let ret = Array.make_matrix (image.height) (image.width) {x = 0 ; y = 0 ; typ = 3} in
	let is_border x y = (x = 0) || (x = (image.width - 1)) || (y = 0) || (y = (image.height - 1)) in
	for i = 0 to ((Array.length blocs) - 1) do
		let (x,y) = (blocs.(i).x,blocs.(i).y) in
		ret.(x).(y) <-
			(if is_border x y then
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
	for i = 0 to (image.height - 1) do
		for j = 5 to (image.width - 1) do
				if sps.(i).(j).typ < 3 then
				begin
					if sps.(i).(j).typ = 0 then set_color blue (* Loop *)
					else if sps.(i).(j).typ = 1 then set_color red (* Delta *)
					else if sps.(i).(j).typ = 2 then set_color green; (* Whorl *)
					moveto j (image.height - i);
					draw_circle j (image.height - i) 2
				end;
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
let filter_test image kernel =
	let bw_img = imageToGreyScale image in
	let m = applyFilter bw_img.matrix kernel in
	let last = matrixApply rgb_of_greyscale m in
	open_graph (getFormat image.width image.height);
	dessiner_image last;;
	
