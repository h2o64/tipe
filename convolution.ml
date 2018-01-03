module type CONVOLUTION =
  sig
		val gaussian_kernel : float Images.matrix
		val gaussian_special_kernel : float Images.matrix
		val sharpen_kernel : float Images.matrix
		val edge1_kernel : float Images.matrix
		val edge2_kernel : float Images.matrix
		val edge3_kernel : float Images.matrix
		val unsharp_masking_kernel : float Images.matrix
		val convolve : int -> int -> float Images.matrix -> float Images.matrix -> float
		val convolve_matrix : float Images.matrix -> float Images.matrix -> float Images.matrix
		val applyFilter : float Images.matrix -> float Images.matrix -> float Images.matrix
  end;;

module Convolution : CONVOLUTION =
  struct
		(* Image Convolution - Kernel Collection *)
		let (gaussian_kernel : float Images.matrix) = [| (* Size = 5 *)
		[|0.012841;0.026743;0.03415;0.026743;0.012841|];
		[|0.026743;0.055697;0.071122;0.055697;0.026743|];
		[|0.03415;0.071122;0.090818;0.071122;0.03415|];
		[|0.026743;0.055697;0.071122;0.055697;0.026743|];
		[|0.012841;0.026743;0.03415;0.026743;0.012841|];
		|];;
		let (gaussian_special_kernel : float Images.matrix) = [| (* Sigma = 9 | Size = 7 *)
		[|0.019179;0.01978;0.020149;0.020273;0.020149;0.01978;0.019179|];
		[|0.01978;0.020399;0.02078;0.020908;0.02078;0.020399;0.01978|];
		[|0.020149;0.02078;0.021168;0.021299;0.021168;0.02078;0.020149|];
		[|0.020273;0.020908;0.021299;0.02143;0.021299;0.020908;0.020273|];
		[|0.020149;0.02078;0.021168;0.021299;0.021168;0.02078;0.020149|];
		[|0.01978;0.020399;0.02078;0.020908;0.02078;0.020399;0.01978|];
		[|0.019179;0.01978;0.020149;0.020273;0.020149;0.01978;0.019179|];
		|];;
		let (sharpen_kernel : float Images.matrix) = [|
		[|0.;-1.;0.|];
		[|-1.;5.;-1.|];
		[|0.;5.;-1.|];
		|];;
		let (edge1_kernel : float Images.matrix) = [|
		[|1.;0.;-1.|];
		[|0.;0.;0.|];
		[|-1.;0.;1.|];
		|];;
		let (edge2_kernel : float Images.matrix) = [|
		[|0.;1.;0.|];
		[|1.;-4.;1.|];
		[|0.;1.;0.|];
		|];;
		let (edge3_kernel : float Images.matrix) = [|
		[|-1.;-1.;-1.|];
		[|-1.;8.;-1.|];
		[|-1.;-1.;-1.|];
		|];;
		let (unsharp_masking_kernel : float Images.matrix) = [| (* Size = 5 *)
		[|0.012841;0.026743;0.03415;0.026743;0.012841|];
		[|0.026743;0.055697;0.071122;0.055697;0.026743|];
		[|0.03415;0.071122;1.859375;0.071122;0.03415|];
		[|0.026743;0.055697;0.071122;0.055697;0.026743|];
		[|0.012841;0.026743;0.03415;0.026743;0.012841|];
		|];;

		(* Do convolution on only one pixel *)
		let convolve i j kernel image_matrix =
			let tmp = ref 0. in
			let r = Array.length kernel in (* Kernel is square *)
			let (h,w) = ((Array.length image_matrix),(Array.length image_matrix.(0))) in
			for m = 0 to (r - 1) do
				for n = 0 to (r - 1) do
					(* Use zero-padding to extend the image *)
					let (a,b) = ((i + m - (r/2)),(j + n - (r/2))) in
					if not((a < 0) || (b < 0) || (a > (h-1)) || (b > (w-1))) then
						tmp := !tmp +.(kernel.(m).(n)*.image_matrix.(a).(b))
				done;
			done;
			!tmp;;

		(* Convolve whole matrix *)
		let convolve_matrix kernel m =
				let (h,w) = ((Array.length m),(Array.length m.(0))) in
				let ret = Array.make_matrix h w 0. in
				for i = 0 to (h - 1) do
					for j = 0 to (w - 1) do
						ret.(i).(j) <- (convolve i j kernel m)
					done;
				done;
				ret;;

		(* Apply filter on an image *)
		let applyFilter matrix kernel = convolve_matrix kernel matrix;;
	end
