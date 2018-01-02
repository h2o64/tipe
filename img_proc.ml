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
					let isBorder = ref false in
					(* Check if we are on the border *)
					if (h-1) < (!i+bloc_size) then isBorder := true;
					if (w-1) < (!j+bloc_size) then isBorder := true;
					(* Process calculation if not on border *)
					if not !isBorder then
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
						variance := !sum /. bloc_size_sqrd;
					(* Wipe out the backgroud *)
					if (!variance > threshold) || !isBorder then
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


	end
