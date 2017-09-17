(* Open image to analyse *)
(* let test_image = import_image "../Poincare_Index/fingerprint.jpg" *)

(* Types *)
type matrix = float array array;;
type bloc_size = int;;
type bw_image = {height : int ; width : int ; mutable matrix : matrix};;
type sp = {mutable x : int ; mutable y : int ; mutable typ : int};; (* 0 = loop | 1 = delta | 2 = whorl | 3 = nothing*)

(* Convert RGB to greyscale *)
let rgbToGreyScale color = (float_of_int (color.r + color.g + color.b))/.3.;;

(* Convert whole image to greyscale *)
let imageToGreyScale (image : Images.image) =
		let (h,w) = (image.height,image.width) in
		let ret = Array.make_matrix h w 0. in
		for i = 0 to (h-1) do
			for j = 0 to (w-1) do
				ret.(i).(j) <- rgbToGreyScale (rgbint_to_color image.matrix.(i).(j))
			done;
		done;
		({height = h; width = w; matrix = ret} : bw_image);;

(* Troncate image for great bloc size *)
let troncateImage (image : Images.image) (bloc_size : bloc_size) =
	let h = image.height - (image.height mod bloc_size) in
	let w = image.width - (image.width mod bloc_size) in
	let ret = (Array.make_matrix h w 0) in
	for i = 0 to (h-1) do
		for j = 0 to (w-1) do
			ret.(i).(j) <- image.matrix.(i).(j)
		done;
	done;
	({ height = h ; width = w ; matrix = ret } : image);;

(* Image Convolution - Gaussian filter *)
let (gaussian_kernel : matrix) = [| (* Size = 5 *)
[|0.012841;0.026743;0.03415;0.026743;0.012841|];
[|0.026743;0.055697;0.071122;0.055697;0.026743|];
[|0.03415;0.071122;0.090818;0.071122;0.03415|];
[|0.026743;0.055697;0.071122;0.055697;0.026743|];
[|0.012841;0.026743;0.03415;0.026743;0.012841|];
|];;

(* Do convolution on only one pixel *)
let convolve i j (kernel : matrix) (image_matrix : matrix) r =
	let tmp = ref 0. in
	for kernel_i = -r to r do
		for kernel_j = -r to r do
			let x = i + kernel_i in
			let y = j + kernel_j in
			tmp := !tmp +.
				(image_matrix.(x).(y)*.kernel.(kernel_i + r).(kernel_j + r))
		done;
	done;
	(-1.) *. !tmp;;

(* Convolve whole matrix *)
let convolve_matrix (kernel : matrix) (m: matrix) =
		let r = ((Array.length kernel) - 1)/2 in (* Kernel is square *)
		let (h,w) = ((Array.length m),(Array.length m.(0))) in
		let ret = m in
		for i = r to (h - 1 - r) do
			for j = r to (w - 1 - r) do
				ret.(i).(j) <- (convolve i j kernel m r)
			done;
		done;ret;;

(* Apply Gaussian filter on image *)
let applyGaussianFilter (image : bw_image) =
		image.matrix<-(convolve_matrix gaussian_kernel image.matrix);;

(* Get all surrounding blocks *)
let getSurrounding i j image_bw (bloc_size : bloc_size)=
	let ret = Array.make_matrix bloc_size bloc_size 0. in
	for k = 0 to 2 do
		for l = 0 to 2 do
			ret.(k).(l) <- image_bw.matrix.(i + (k - 1)).(j + (l-1));
		done;
	done;(ret : matrix);;

(* Make matrix array for each bloc_size*bloc_size blocs *)
let makeBlocList img (bloc_size : bloc_size) =
		let (h,w) = (img.height,img.width) in
		let ret = Array.make (h*w)
						 ((Array.make_matrix bloc_size bloc_size 0. : matrix)) in
		for i = 1 to (h-2) do
			for j = 1 to (w-2) do
				ret.(i*w+j) <- (getSurrounding i j img bloc_size)
			done;
		done;
		ret;;

(* Uses Sobel operator *)
let pi = 4. *. atan 1.
let hX = [|[|-1.;0.;1.|];[|-2.;0.;2.|];[|-1.;0.;1.|]|];;
let hY = [|[|-1.;-2.;-1.|];[|0.;0.;0.|];[|1.;2.;1.|]|];; (* Transposée de gX *)
let getAngles m =
	let gX = convolve_matrix hX m in
	let gY = convolve_matrix hY m in
	let ret = Array.make_matrix 3 3 0. in
	for i = 0 to 2 do
		for j = 0 to 2 do
			ret.(i).(j) <- (atan2 gY.(i).(j) gX.(i).(j))
		done;
	done;(ret : matrix);;

(* Sum angles and get the sg type *)
let sumAngles i j (matrix : matrix) =
	let error = 15 in (* 30° of error allowance *)
	let sum = ref 0. in
	let ret = {x = i ; y = j ; typ = 4} in
	for k = 0 to 2 do
		for l = 0 to 2 do
			if (k != 1) && (l != 1) then sum := !sum +. matrix.(k).(l)
		done;
	done;
	let deg_sum = (int_of_float ((!sum*.180.)/.pi)) in (* mod 360 in *)
	if (abs (deg_sum - 180)) < error then ret.typ<-(1)
	else if (abs (deg_sum + 180)) < error then ret.typ<-(2)
	else if (abs (deg_sum - 360)) < error then ret.typ<-(3);
	ret;;

(* Get coordonates from array position *)
let getCoordonates ind w = ((ind/w),(ind mod w));;

(* Get all the singularity points *)
let poincare_index (image : bw_image) =
	applyGaussianFilter image; (* Apply Gaussian filter *)
	let blocs = makeBlocList image 8 in
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
	let great_image = (troncateImage image 8) in
	let sps = poincare_index (imageToGreyScale great_image) in
	open_graph (getFormat great_image.width great_image.height);
	draw_image (make_image great_image.matrix) 0 0;
	set_color red;
	for i = 5 to (great_image.height - 5) do
		for j = 5 to (great_image.width - 5) do
				if sps.(i).(j).typ < 4 then
					(moveto j i;
					draw_circle j i 5);
		done;
	done;
	let _ = read_key() in close_graph();;

(* List singularity points *)
let list_sp image =
	let great_image = (troncateImage image 8) in
	let sps = poincare_index (imageToGreyScale great_image) in
	for i = 5 to (great_image.height - 5) do (* 5 = kernel width *)
		for j = 5 to (great_image.width - 5) do
			if sps.(i).(j).typ < 4 then
				begin
					if sps.(i).(j).typ = 0 then print_string "Loop at" (* Loop *)
					else if sps.(i).(j).typ = 1 then print_string "Delta at" (* Delta *)
					else if sps.(i).(j).typ = 2 then print_string "Whorl at"; (* Whorl *)
					print_string (getFormat i j);
					print_string "\n";
				end;
		done;
	done;;
