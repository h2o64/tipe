(* module type CONVOLUTION =
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

module Convolution : CONVOLUTION = *)
module Convolution =
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
			let (h,w) = Images.getHW image_matrix in
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
				let (h,w) = Images.getHW m in
				let ret = Array.make_matrix h w 0. in
				for i = 0 to (h - 1) do
					for j = 0 to (w - 1) do
						ret.(i).(j) <- (convolve i j kernel m)
					done;
				done;
				ret;;

		(* Matrix naïve multiplication *)
		let matrix_multiply x y =
			let h = Bigarray.Array2.dim1 x in
			let w = Bigarray.Array2.dim2 x in
			let ret = FFT.Array2.create FFT.complex Bigarray.c_layout h w in
			for i = 0 to h-1 do
				for j = 0 to w-1 do
					ret.{i,j} <- Complex.mul x.{i,j} y.{i,j}
				done;
			done;ret;;

		(* Pad an image with 0 *)
		let padImg m n ker_size =
			let (h,w) = (Bigarray.Array2.dim1 m,Bigarray.Array2.dim2 m) in
      let ret = FFT.Array2.create FFT.complex Bigarray.c_layout n n in
			Bigarray.Array2.fill ret Complex.zero;
			(* Copy image in ret *)
			for i = ker_size to (h-1)+ker_size do
				for j = ker_size to (w-1)+ker_size do
					ret.{i,j} <- m.{i-ker_size,j-ker_size}
				done;
			done;
			(* Fill borders with some pseudo-interpolation *)
			for side = 0 to 1 do
				for i = ker_size to (h-1-ker_size) do
					let j = ref (if side = 0 then ker_size-1 else (w-1+1)) in
					while (!j >= 0) && (!j < (w+ker_size)) do
						if side = 0 then
							(let local_grad = Complex.sub ret.{i,!j+2}  ret.{i,!j+1} in
							ret.{i,!j} <- Complex.add ret.{i,!j+1} local_grad;
							j := !j - 1)
						else
							(let local_grad = Complex.sub ret.{i,!j-2}  ret.{i,!j-1} in
							ret.{i,!j} <- Complex.add ret.{i,!j-1} local_grad;
							j := !j + 1);
					done;
				done;
			done;
			for side = 0 to 1 do
				for j = ker_size to (w-1-ker_size) do
					let i = ref (if side = 0 then ker_size-1 else (h-1+1)) in
					while (!i >= 0) && (!i < (h+ker_size)) do
						if side = 0 then
							(let local_grad = Complex.sub ret.{!i+2,j}  ret.{!i+1,j} in
							ret.{!i,j} <- Complex.add ret.{!i+1,j} local_grad;
							i := !i - 1)
						else
							(let local_grad = Complex.sub ret.{!i-2,j}  ret.{!i-1,j} in
							ret.{!i,j} <- Complex.add ret.{!i-1,j} local_grad;
							i := !i + 1);
					done;
				done;
			done;
			ret;;

		(* FFTW Convolution *)
		let convolve_matrix_fft_ba kernel m =
			let (h,w) = (Bigarray.Array2.dim1 m,Bigarray.Array2.dim2 m) in
			let (h_k,w_k) = Images.getHW kernel in
			let n = (max h w) + h_k in
			let input_image = padImg m n h_k in
      let input_kernel = FFT.Array2.create FFT.complex Bigarray.c_layout n n in
      let output_image = FFT.Array2.create FFT.complex Bigarray.c_layout n n in
      let output_kernel = FFT.Array2.create FFT.complex Bigarray.c_layout n n in
			(* Compute FFT for each matrix *)
      let dft_image = FFT.Array2.dft ~meas:FFT.Estimate FFT.Forward input_image output_image in
      let dft_kernel = FFT.Array2.dft ~meas:FFT.Estimate FFT.Forward input_kernel output_kernel in
			(* Do kernel more manualy - It's small anyway *)
			Bigarray.Array2.fill input_kernel Complex.zero;
			for i = 0 to (h_k-1) do
				for j = 0 to (w_k-1) do
					input_kernel.{i,j} <- { re = kernel.(i).(j); im = 0.0 };
				done;
			done;
			(* Exec plans *)
			FFT.exec dft_image;
			FFT.exec dft_kernel;
			(* Multiply and return *)
			let mul_ret = matrix_multiply output_image output_kernel in
      let output_ret = FFT.Array2.create FFT.complex Bigarray.c_layout n n in
      let dft_ret = FFT.Array2.dft ~meas:FFT.Estimate FFT.Backward mul_ret output_ret in
			FFT.exec dft_ret;
			(* Cut the party we want *)
			let ret = FFT.Array2.create FFT.complex Bigarray.c_layout h w in
			for i = h_k to (h-1) do
				for j = h_k to (w-1) do
					ret.{i-h_k,j-h_k} <- output_ret.{i+h_k/2,j+h_k/2};
				done;
			done;
			ret;;

		(* Convert bigarray to 'a array array *)
		let ba_to_matrix (m : (Complex.t, FFT.complex_elt, Bigarray.c_layout) Bigarray.Array2.t) =
			let (h,w) = (Bigarray.Array2.dim1 m,Bigarray.Array2.dim2 m) in
			let ret = Array.make_matrix h w 0. in
			for i = 0 to h-1 do
				for j = 0 to w-1 do
					ret.(i).(j) <- m.{i,j}.re;
				done;
			done;
			ret;;

		(* Convolve with 'a array array *)
		let convolve_matrix_fft kernel m =
			(* Create image bigarray *)
      let image = Bigarray.Array2.of_array FFT.complex Bigarray.c_layout (Images.applyFunctMatrix m (fun x -> ({ re = x; im = 0.0 } : Complex.t))) in
			let conv = convolve_matrix_fft_ba kernel image in
			ba_to_matrix conv;;

		(* let test_convol m kernel =
			let tmp = ref m in
			Testing.displayAnyMatrix !tmp;
			for i = 0 to 40 do
				(tmp := (convolve_matrix_fft kernel !tmp);
				Testing.align_matrix !tmp;
				Testing.displayAnyMatrix !tmp);
			done;; *)

		(* Apply filter on an image *)
		let applyFilter matrix kernel = convolve_matrix_fft kernel matrix;;
	end
