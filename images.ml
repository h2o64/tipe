module type IMAGES =
  sig
    type 'a matrix = 'a array array
    type 'a bloc = { x : int; y : int; matrix : 'a matrix; }
    type 'a pixel_blocs = 'a bloc list
    type 'a image = {
      height : int;
      width : int;
      mutable matrix : 'a matrix;
    }
    val getHeight : 'a matrix -> int
    val getWidth : 'a matrix -> int
    val getHW : 'a matrix -> int * int
    val matrixApply : ('a -> 'b) -> 'a matrix -> 'b matrix
    val import_image : string -> Graphics.color image
    val getFormat : int -> int -> string
    val color_of_rgbint : Graphics.color -> int * int * int
    val greyscale_of_rgb : Graphics.color -> float
    val rgb_of_greyscale : float -> int
    val imageToGreyScale : Graphics.color image -> float image
    val bwimageToImage : float image -> int image
    val getSurrounding :
      int -> int -> 'a matrix -> int -> int -> int -> 'a matrix
    val makeBlocList : 'a matrix -> int -> 'a bloc array
    val troncateImage : 'a image -> int -> 'a image
    val transpose : 'a matrix -> 'a matrix
    val getBlocPos : int -> int -> int -> (int * int) * (int * int)
    val createMatrixOfMatrix :
      int -> int -> int -> 'a -> 'a array array array array
    val cutInBlocs : 'a matrix -> int -> 'a array array array array
    val getMatrixAv : float matrix -> float
    val applyFunctMatrix : 'a matrix -> ('a -> 'b) -> 'b array array
    val applyFunctMatrix_d :
      'a matrix -> 'b matrix -> ('a -> 'b -> 'c) -> 'c array array
    val copyMatrix : 'a array array -> 'a array array
    val areThereNonZeros_aux : int array array -> int -> bool -> bool
    val areThereNonZeros : int array array -> bool
    val absDiff : int matrix -> int matrix -> int array array
  end;;

module Images : IMAGES =
  struct
		(* Structures *)
		type 'a matrix = 'a array array;;
		type 'a bloc = {x : int ; y : int ; matrix : 'a matrix};;
		type 'a pixel_blocs = 'a bloc list;;
		type 'a image = {height : int ; width : int ; mutable matrix : 'a matrix};;

		(* Get height and width of an image *)
		let getHeight (img : 'a matrix) = Array.length img;;
		let getWidth (img : 'a matrix) = Array.length img.(0);;
		let getHW m = (getHeight m,getWidth m);;

		(* Apply a function to each element of a matrix *)
		let matrixApply (f : 'a -> 'b) (matrix : 'a matrix) =
			let (h,w) = getHW matrix in
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
			let (h,w) = getHW matrix in
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
			let (h,w) = getHW matrix in
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
			let (h,w) = getHW matrix in
			let ret = ref 0. in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					ret := !ret +. matrix.(i).(j)
				done;
			done;(!ret/.(float_of_int (h*w)));;

		(* Apply function on matrix *)
		let applyFunctMatrix m f =
			let (h,w) = getHW m in
			let ret = Array.make_matrix h w (f m.(0).(0)) in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					ret.(i).(j) <- f m.(i).(j);
				done;
			done;ret;;

		(* Apply function on matrix *)
		let applyFunctMatrix_d a b f =
			let (h,w) = getHW a in
			if not ((h,w) =  getHW b) then failwith "applyFunctMatrix_d: Not same size";
			let ret = Array.make_matrix h w (f a.(0).(0) b.(0).(0)) in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					ret.(i).(j) <- f a.(i).(j) b.(i).(j);
				done;
			done;ret;;

		(* Copy matrix *)
		let copyMatrix m =
			let f x = x in
			applyFunctMatrix m f;;

		(* Are there zeros ? *)
		let rec areThereNonZeros_aux m i b =
			if i < 0 then false
			else if (b = true) then true
			else (areThereNonZeros_aux m (i-1) (Array.mem 1 m.(i)));;
		let areThereNonZeros m = areThereNonZeros_aux m ((Array.length m)-1) false;;

		(* Get abs difference between two matrix *)
		let absDiff a b =
			let f a b = abs (b-a) in
			applyFunctMatrix_d a b f;;

  end
