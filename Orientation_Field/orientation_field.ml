(* Open image to analyse *)
let test_image = import_image "../Poincare_Index/fingerprint.jpg";;

(* Type *)
type vector = { mutable a : int*int ; mutable b : int*int };;

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
		ret;;

(* Uses Sobel operator *)
(* https://en.wikipedia.org/wiki/Prewitt_operator *)
(* https://en.wikipedia.org/wiki/Sobel_operator *)
let pi = 4. *. atan 1.
let hX = [|[|1.;0.;-1.|];[|2.;0.;-2.|];[|1.;0.;-1.|]|];; (* Sobel *)
let hY = [|[|1.;2.;1.|];[|0.;0.;0.|];[|-1.;-2.;-1.|]|];; (* Transposée de gX *)
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
	
(* Create vector array *)
let vectorList matrix =
	let pi = 4. *. atan 1. in
	let deg_of_rad x = int_of_float ((x*.180.)/.pi) in
	let bloc_size = 16 in
	let blocked  = cutInBlocs matrix bloc_size in
	let (h,w) = (Array.length blocked),(Array.length blocked.(0)) in
	let vector_array = Array.make (h*w) ({a = (-1,-1) ; b = (-1,-1)}) in
	for i = 0 to (h-1) do
		for j = 0 to (w-1) do
			let av = getMatrixAv (getAngles blocked.(i).(j)) in
			(* Descrete values : multiples of 45° *)
			let deg_av = (abs (deg_of_rad av))/45 in
			let (x,y) = (i*bloc_size+1,j*bloc_size+1) in (* Co of the middle point of the bloc *)
			if (deg_av = 0) then vector_array.(i*w+j)<-{a = (-1,-1); b = (-1,-1)}
			else if ((deg_av mod 4) = 1) then vector_array.(i*w+j)<-{a = (x+1,y-1); b = (x-1,y+1)}
			else if ((deg_av mod 4) = 2) then vector_array.(i*w+j)<-{a = (x,y-1); b = (x,y+1)}
			else if ((deg_av mod 4) = 3) then vector_array.(i*w+j)<-{a = (x-1,y-1); b = (x+1,y+1)}
			else if ((deg_av mod 4) = 0) then vector_array.(i*w+j)<-{a = (x-1,y); b = (x+1,y)}
		done;
	done; vector_array;; 
			

(* Draw vectors from vector array *)
let drawVectors (vector_array : vector array) =
	set_line_width 3; (* Set line width to 1 *)
	set_color red; (* Set color to red *)
	for i = 0 to ((Array.length vector_array) - 1) do
		let (x,y) = vector_array.(i).a in
		let (a,b) = vector_array.(i).b in
		if not((a = -1) || (b = -1) || (x = -1) || (y = -1)) then (* NOTE: Use Array.exists in 4.03 *)
			(draw_segments [|x,y,a,b|])
	done;();;

(* Open image and draw vectors *)
let display_orient image =
	let vector_array = vectorList (imageToGreyScale image).matrix in
	(* open_graph (getFormat image.width image.height); *)
	open_graph " 800x800"; (* Temporary *)
	draw_image (make_image image.matrix) 0 0;
	drawVectors vector_array;
	let _ = read_key() in close_graph();;
