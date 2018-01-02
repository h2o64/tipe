(* module type IMAGE_PROCESSING =
  sig

  end;;

module Image_Processing : IMAGE_PROCESSING = *)
module Image_Processing =
  struct
		(* Mean and variance based method of segmentation *)
		let segmentation m bloc_size threshold =
			let (h,w) = ((Array.length m),(Array.length m.(0))) in
			let ret = Array.make_matrix h w 0. in
			let bloc_size_sqrd = float_of_int (bloc_size*bloc_size) in
			let i = ref 0 in
			while !i < h do
				let j = ref 0 in
				while !j < w do
					let variance = ref (threshold +. 1.) in
					(* Process calculation if not on border *)
					if not (((h-1) < (!i+bloc_size)) || ((w-1) < (!j+bloc_size)))  then
						(let max_h = (!i+bloc_size)-1 in
						let max_w = (!j+bloc_size)-1 in
						(* Sum blocs intensities *)
						let dbg = ref 0 in
						let sum = ref 0. in
						for k = !i to max_h do
							for l = !j to max_w do
								sum := !sum +. m.(k).(l);
								dbg := !dbg + 1;
							done;
						done;
						(* Get bloc mean *)
						let mean = !sum /. bloc_size_sqrd in
						(* Get bloc variance *)
						let sum = ref 0. in
						for k = !i to max_h do
							for l = !j to max_w do
								sum := !sum +. ((m.(k).(l) -. mean)**2.);
							done;
						done;
						variance := !sum /. bloc_size_sqrd);
					(* Wipe out the backgroud *)
					if (!variance > threshold) then
						(for k = !i to (min (!i+bloc_size) (h-1)) do
							for l = !j to (min (!j+bloc_size) (w-1)) do
								ret.(k).(l) <- m.(k).(l)
							done;
						done);
					j := !j + bloc_size;
				done;
				i := !i + bloc_size
			done;
		ret;;

	(* Image normalisation with histogram equalization *)
	let normalisation m =
		let (h,w) = ((Array.length m),(Array.length m.(0))) in
		(* Get occurence of each grey level *)
		let occurs = Array.make 256 0 in
		for i = 0 to (h-1) do
			for j = 0 to (w-1) do
				let value = occurs.(int_of_float (m.(i).(j))) in
				occurs.(int_of_float (m.(i).(j))) <- value + 1;
			done;
		done;
		(* Get the transformation *)
		let transf = Array.make 256 0. in
		let size = float_of_int (h*w) in
		let tmp = (255. /. size) in
		for i = 0 to 255 do
			let sum = ref 0 in
			for j = 0 to i do
				sum := !sum + occurs.(j)
			done;
			transf.(i) <- tmp *. (float_of_int !sum)
		done;
		(* Transform the image *)
		let ret = Array.make_matrix h w 0. in
		for i = 0 to (h-1) do
			for j = 0 to (w-1) do
				ret.(i).(j) <- transf.(int_of_float m.(i).(j))
			done;
		done;
		ret;;

	end
