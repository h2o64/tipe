(* Open image to analyse *)
let test_image = import_image "../Orientation_Field/fingerprint.jpg"

(* Variable *)
let global_bloc_size = 3;;

(* Bloc type *)
type bloc = Images.color array;;

(* Get necessary bloc size *)
(* TO BE IMPROVED *)
let getBlocSize img bloc_size = (img.height - (img.height mod bloc_size), img.width - (img.width mod bloc_size));;

(* Get bloc number based on matrix coordonates *)
let getBlocCo i j h w bloc_size = (i mod h/bloc_size)*(w/bloc_size)+(j mod w/bloc_size);;

(* Get the pixel number inside the bloc *)
let getBlocNum i j bloc_size = bloc_size*(i mod (bloc_size)) + (j mod (bloc_size));;

(* Make a bloc bloc_size*bloc_size list *)
let makeBlocList img bloc_size =
		let (h,w) = getBlocSize img bloc_size in
		let ret = Array.make (((h*w)/bloc_size)+1)
						 (Array.make (bloc_size*bloc_size) {r = 0; g = 0; b = 0; a = 0}) in
		for i = 0 to (h-1) do
			for j = 0 to (w-1) do
				let cur_pix = rgbint_to_color (img.matrix.(i).(j)) in
				ret.(getBlocCo i j h w bloc_size).(getBlocNum i j bloc_size) <- cur_pix
			done;
		done;
		ret;;

(* Sanitizer - I'm checking if everything is okay *)
let sanitizer checkme bloc_size =
		let ret = ref true in
		for i = 0 to (Array.length checkme)-1 do
			ret := !ret && ((Array.length checkme.(i)) = bloc_size*bloc_size)
		done;
		!ret;;

(* Get angles *)
(* Uses Sobel operator *)
let hX = [|[|-1;0;1|];[|-2;0;2|],[|-1;0;1|]|];;
let hY = [|[|-1;-2;-1|];[|0;0;0|],[|1;2;1|]|];; (* Transposé de gX *)
let getAngles matrix = 
	
	

	
