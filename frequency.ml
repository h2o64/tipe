module type FREQUENCY =
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
    val frequency_map_hos : float Images.matrix -> int -> float array array
  end;;

module Frequency : FREQUENCY =
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
		let frequency_map_hos m bloc_size =
			let sign_map = get_signature_map m bloc_size in
			let (h,w) = Images.getHW sign_map in
			let ret = Array.make_matrix h w 0. in
			let alpha = 1.2 in
			let get_ft arr =
				let input = FFT.Array1.of_array FFT.complex Bigarray.c_layout arr in
			let output = FFT.Array1.create FFT.complex Bigarray.c_layout ((bloc_size)*2) in
			let dft = FFT.Array1.dft FFT.Forward input output in
			FFT.exec dft;
				output in
			let getSpectrumMagn sign_t =
				(* Get magnitude of fourrier transform *)
				let magn = Array.make (bloc_size*2) 0. in
				for k = 0 to (bloc_size*2)-1 do
					magn.(k) <- Complex.norm sign_t.{k};
				done; magn in
			let getPowerSpectrum sign_t =
				(* Get spectrum from a fourrier transform *)
				let magn = getSpectrumMagn sign_t in
				(Array.map (fun x -> x**2.) magn) in
			let arr_mul a b =
				(* Multiply two arrays *)
				let ret = Array.make (Array.length a) 0. in
				for i = 0 to (Array.length a)-1 do
					ret.(i) <- a.(i) *. b.(i);
				done; ret in
			let getFundamental arr =
				let cur_m = ref arr.(1) in
				let cur_m_ind = ref 0 in
				for i = 2 to ((Array.length arr)-1) do
					if arr.(i) > !cur_m then
						(cur_m := arr.(i);
						cur_m_ind := (i-1));
				done; (float_of_int !cur_m_ind) in
			let complex_array t = Array.map (fun x -> ({ re = x; im = 0.0 } : Complex.t)) t in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					let p_arr = complex_array sign_map.(i).(j) in
					let f_arr = complex_array (Array.map (fun x->alpha*.x) sign_map.(i).(j)) in
					let f2_arr = complex_array (Array.map (fun x->2.*.x) sign_map.(i).(j)) in
					let f3_arr = complex_array (Array.map (fun x->3.*.x) sign_map.(i).(j)) in
					(* Execute DFTs - TODO: Optimize with howmany arguments *)
					let p_t = getPowerSpectrum (get_ft p_arr) in
					let f_t = getSpectrumMagn (get_ft f_arr) in
					let f2_t = getSpectrumMagn (get_ft f2_arr) in
					let f3_t = getSpectrumMagn (get_ft f3_arr) in
					let coef = min (f_t) (max f2_t f3_t) in
					let m_f = arr_mul p_t coef in
					ret.(i).(j) <- getFundamental m_f; (* Get fundamental freq of the power spect *)
				done;
			done;ret;;
	end
