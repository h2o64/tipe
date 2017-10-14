(* Libraries *)
open Image_magick;;

(* Structures *)
type 'a matrix = 'a array array;;
type 'a bloc = {x : int ; y : int ; matrix : 'a matrix};;
type 'a pixel_blocs = 'a bloc list;;
type 'a image = {height : int ; width : int ; mutable matrix : 'a matrix};;

(* Get height and width of an image *)
let getHeight (img : 'a matrix) = Array.length img;;
let getWidth (img : 'a matrix) = Array.length img.(0);;

(* Apply a function to each element of a matrix *)
let matrixApply (f : 'a -> 'b) (matrix : 'a matrix) =
	let (h,w) = (Array.length matrix,Array.length matrix.(0)) in
	let ret = Array.make_matrix h w (f matrix.(0).(0)) in
	for i = 0 to (h- 1) do
		for j = 0 to (w - 1) do
			ret.(i).(j) <- f (matrix.(i).(j))
		done;
	done;(ret : 'b matrix);;

(* Import an image *)
let import_image file = let matrix = lire_image file in
		{height = getHeight matrix; width = getWidth matrix; matrix = matrix};;

(* Get the right image format *)
let getFormat height width =
	let s_height = string_of_int height in
	let s_width = string_of_int width in
	String.concat "" [" ";s_height;"x";s_width];;

(* Convert RGB integer to color type *)
let color_of_rgbint (num : Graphics.color) =
	(* Red/Green/Blue *)
	let b = num mod 256 in
	let g = (num/256) mod 256 in
	let r = (num/256/256) mod 256 in
	(r,g,b);;

(* Convert RGB to greyscale *)
let greyscale_of_rgb pix = let (r,g,b) = color_of_rgbint pix in
			(float_of_int (r + g + b))/.3.;;

(* Convert greyscale to RGB *)
let rgb_of_greyscale pix = (int_of_float pix) * 0x00010101;;

(* Convert whole image to greyscale *)
let imageToGreyScale image = let new_m = (matrixApply greyscale_of_rgb image.matrix) in
	{ height = image.height ; width = image.width ; matrix = new_m };;

(* Convert whole greyscale image to RGB image *)
let bwimageToImage image = let new_m = (matrixApply rgb_of_greyscale image.matrix) in
	{ height = image.height ; width = image.width ; matrix = new_m };;

(* Get all surrounding blocks *)
let getSurrounding i j (matrix : 'a matrix) h w bloc_size =
	let ret = (Array.make_matrix bloc_size bloc_size matrix.(0).(0)) in
	for k = 0 to (bloc_size-1) do
		for l = 0 to (bloc_size-1) do
			let a = i+(k-1) in
			let b = j+(l-1) in
			if not((a < 0) || (b < 0) || (a >= h) || (b >= w)) then
				ret.(k).(l) <- matrix.(a).(b);
		done;
	done;(ret : 'a matrix);;

(* Make matrix array for each bloc_size*bloc_size blocs *)
let makeBlocList matrix bloc_size =
	let (h,w) = ((Array.length matrix),(Array.length matrix.(0))) in
	let ret = Array.make (h*w)
			{x = -1; y = -1; matrix = (Array.make_matrix bloc_size bloc_size matrix.(0).(0))}  in
		for i = 0 to (h-1) do
			for j = 0 to (w-1) do
				ret.(i*w+j) <- {x = i ; y = j ; matrix = (getSurrounding i j matrix h w bloc_size)}
			done;
		done;
		ret;;
	
(* Troncate image for great bloc size *)
let troncateImage image bloc_size =
	let h = image.height - (image.height mod bloc_size) in
	let w = image.width - (image.width mod bloc_size) in
	let ret = (Array.make_matrix h w image.matrix.(0).(0)) in
	for i = 0 to (h-1) do
		for j = 0 to (w-1) do
			ret.(i).(j) <- image.matrix.(i).(j)
		done;
	done;
	{ height = h ; width = w ; matrix = ret };;

(* Transpose a matrix *)
let transpose (m : 'a matrix)=
	let n = Array.make_matrix (Array.length m.(0)) (Array.length m) m.(0).(0) in
	for i = 0 to (Array.length m - 1) do
		let row_i = m.(i) in
			for j = 0 to (Array.length row_i - 1) do
				n.(j).(i) <- row_i.(j)
			done;
		done;
	(n : 'a matrix);;

(* Get pixel position in bloc matrix *)
let getBlocPos i j bloc_size =
	let bloc_co = ((i/bloc_size),(j/bloc_size)) in
	let co_in_bloc = ((i mod bloc_size),(j mod bloc_size)) in
	(bloc_co,co_in_bloc);;

(* Create 'a array array array array *)
let createMatrixOfMatrix h w bloc_size a =
	let ret = ref [||] in
	for i = 0 to (h-1) do
		let tmp = ref [||] in
		for j = 0 to (w-1) do
			tmp := Array.append !tmp ([|Array.make_matrix bloc_size bloc_size a|]);
		done;
		ret := Array.append !ret [|!tmp|];
	done;!ret;;

(* Make matrix array for each bloc_size*bloc_size blocs *)
let cutInBlocs matrix bloc_size =
	let (h,w) = ((Array.length matrix),(Array.length matrix.(0))) in
	let ret = createMatrixOfMatrix (h/bloc_size) (w/bloc_size)
					bloc_size matrix.(0).(0) in
		for i = 0 to (h-1-(h mod bloc_size)) do
			for j = 0 to (w-1-(w mod bloc_size)) do
				let ((bloc_x,bloc_y),(x_in_bloc,y_in_bloc)) = getBlocPos i j bloc_size in
				(ret.(bloc_x).(bloc_y)).(x_in_bloc).(y_in_bloc) <- matrix.(i).(j)
			done;
		done;
		ret;;

(* Get matrix average value *)
let getMatrixAv matrix =
	let (h,w) = ((Array.length matrix),(Array.length matrix.(0))) in
	let ret = ref 0. in
	for i = 0 to (h-1) do
		for j = 0 to (w-1) do
			ret := !ret +. matrix.(i).(j)
		done;
	done;(!ret/.(float_of_int (h*w)));;
