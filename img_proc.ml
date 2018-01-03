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

	(* Kernel from function *)
	let kernelFromFunction size f =
		let kernel = Array.make_matrix size size 0. in
		for i = 0 to (size-1) do
			for j = 0 to (size-1) do
				kernel.(i).(j) <- f (float_of_int (i-size)/.2.) (float_of_int (j-size)/.2.)
			done;
		done;
		kernel;;

	(* Generate the Gabor kernel *)
	let gabor_kernel angle freq bloc_size =
		let c = cos angle in
		let s = sin angle in
		let yangle x y = x*.c +. y*.s in
		let xangle x y = (-1.) *. x *.s +. y*.c in
		let xsigma_sqrd = 16. in
		let ysigma_sqrd = 16. in
		let funct x y =
			(-0.5) *. ((((xangle x y)**2.) /. xsigma_sqrd) +. (((yangle x y)**2.) /. ysigma_sqrd)) in
		let signal x y = cos (2. *. Poincare.pi *. freq *. (xangle x y)) in
		let mul x y = (exp (funct x y)) *. (signal x y) in
		(kernelFromFunction bloc_size mul);;

	(* Gauss fonction *)
	let gauss x y =
		let sigma = 1. in
		((1.) /. (2.*.Poincare.pi*.(sigma**2.)))*.exp((-1.)*.((x**2.)+.(y**2.))/.(2. *. (sigma**2.)));;

	(* Apply gabor kernel *)
	let apply_gabor m bloc_size =
		let (h,w) = ((Array.length m),(Array.length m.(0))) in
		let ret = Array.make_matrix h w 0. in
		let angles = Orientation.smoothMyAngles (Orientation.getAngles m bloc_size) in
		let freqs = Frequency.frequency_map m bloc_size in
		let (h_b,w_b) = ((Array.length angles),(Array.length angles.(0))) in
		let gauss_kernel = kernelFromFunction 3 gauss in
		let new_freqs = Convolution.convolve_matrix gauss_kernel freqs in
		for i = 0 to (h_b-1) do
			for j = 0 to (w_b-1) do
				let kernel = gabor_kernel angles.(i).(j) new_freqs.(i).(j) bloc_size in
				for k = 0 to (bloc_size-1) do
					for l = 0 to (bloc_size-1) do
						let (x,y) = ((i*bloc_size+k),(j*bloc_size+l)) in
						if (x < h) && (y < w) then
							ret.(x).(y) <- (Convolution.convolve x y kernel m);
					done;
				done;
			done;
		done;
		ret;;
	end
