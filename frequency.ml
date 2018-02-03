(* module type FREQUENCY =
  sig
    val signature :
      int ->
      int -> float Images.matrix -> float array array -> int -> int -> float
    val get_signatures :
      int -> int -> float Images.matrix -> int -> float array
    val createMatrixOfArray : int -> int -> int -> 'a -> 'a array array array
    val get_signature_map :
      float Images.matrix -> int -> float array array array
    val average_pics_d : 'a array -> float
    val frequency_map : float Images.matrix -> int -> float array array
  end;;

module Frequency : FREQUENCY = *)
module Frequency =
  struct

		(* Get the signature of the windows (bloc_size,2*bloc_size) centered at (i,j) *)
		let signature i j m angles bloc_size k =
			let (he,wi) = Images.getHW m in
			let x = ref 0. in
			let w = float_of_int bloc_size in
			let angle = angles.((i-1)/bloc_size).((j-1)/bloc_size) in
			let (y,z) = (float_of_int i,float_of_int j) in
			for d = 0 to bloc_size-1 do
				let u = y +. ((float_of_int d) -. (w/.2.)) *. (cos angle) +. ((float_of_int k) -. 1./.2.) *. (sin angle) in
				let v = z +. ((float_of_int d) -. (w/.2.)) *. (sin angle) -. ((float_of_int k) -. 1./.2.) *. (cos angle) in
				let u_int = (int_of_float u) in
				let v_int = (int_of_float v) in
				if (u_int < he) && (v_int < wi) && (u_int > 0) && (v_int > 0) then
					x := !x +. m.(u_int).(v_int);
			done;
			(!x/.w);;

		(* Get the signature *)
		let get_signatures i j m bloc_size =
			let angles = Orientation.smoothMyAngles (Orientation.getAngles_vector m bloc_size) in
			let signature_k k = signature i j m angles bloc_size k in
			let signatures = Array.make (bloc_size*2) (signature_k 0) in
			for l = 0 to (bloc_size*2)-1 do
				signatures.(l)<-signature_k l;
			done;signatures;;

		(* Create 'a array array array *)
		let createMatrixOfArray h w bloc_size a =
			let ret = ref [||] in
			for i = 0 to (h-1) do
				let tmp = ref [||] in
				for j = 0 to (w-1) do
					tmp := Array.append !tmp ([|Array.make bloc_size a|]);
				done;
				ret := Array.append !ret [|!tmp|];
			done;!ret;;

		(* Get signature map *)
		let get_signature_map m bloc_size =
			let (h,w) = Images.getHW m in
			let (h_new,w_new) = (h-1/bloc_size,w-1/bloc_size) in
			let ret = createMatrixOfArray h_new w_new (bloc_size*2) 0. in
			let angles = Orientation.smoothMyAngles (Orientation.getAngles_vector m bloc_size) in
			let i = ref 0 in
			while !i < h do
				let j = ref 0 in
				while !j < w do
					let signature_k k = signature !i !j m angles bloc_size k in
					for l = 0 to (bloc_size*2)-1 do
						ret.((!i-1)/bloc_size).((!j-1)/bloc_size).(l)<-signature_k l;
					done;
					j := !j + bloc_size;
				done;
				i := !i + bloc_size
			done;
			ret;;

		(* Get average pics distance *)
		let average_pics_d tab =
			let length = Array.length tab in
			(* Get all pics *)
			let pics = ref [||] in
			for i = 1 to (length-2) do
				if (tab.(i-1) < tab.(i)) && (tab.(i+1) < tab.(i)) then
					pics := Array.append !pics [|i|];
			done;
			(* Get all distances average *)
			let nb_pics = (Array.length !pics) in
			let ret = ref (-1.) in
			if not (nb_pics = 0) then
				(let distance = ref 0 in
				for j = 1 to (nb_pics-1) do
					distance := !distance + (!pics.(j) - !pics.(j-1)) - 1;
				done;
				ret := ((float_of_int !distance) /. (float_of_int (nb_pics))));
			!ret;;

		(* Get frequency map *)
		let frequency_map m bloc_size =
			let sign_map = get_signature_map m bloc_size in
			let (h,w) = Images.getHW sign_map in
			let ret = Array.make_matrix h w 0. in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					let tmp = (average_pics_d sign_map.(i).(j)) in
					(* Ignore too weak values *)
					if tmp < (10.**(-5.)) then
						ret.(i).(j) <- (0.)
					else
						ret.(i).(j) <- 1. /. tmp;
				done;
			done;ret;;

		(* Higher Order Spectrum Method *)
		open Complex;;
		let frequency_map_hos m bloc_size =
			let sign_map = get_signature_map m bloc_size in
			let (h,w) = Images.getHW sign_map in
			let ret = Array.make_matrix h w 0. in
			let alpha = 1.2 in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
				(* (* Get Fourrier transform of the bloc's signature *)
				let complex_signature = FFT.complex_array_of_array sign_map.(i).(j) in
				let fft_signature_complex = FFT.fft complex_signature in
				let fft_signature = Array.map norm fft_signature_complex in *)
				();
				done;
			done;ret;;
	end
