(* Open Libraries *)
open Convolution;;
open Frequency;;
open Images;;
open Orientation;;
open Poincare;;
open Testing;;

module Image_Processing :
 sig
    val getOptimalThreshold_otsu : float Images.matrix -> int -> float
    val segmentation :
      float Images.matrix -> int -> float -> float array array
    val normalisation : float Images.matrix -> float array array
    val segmentation_gradient :
      float Images.matrix -> int -> float -> bool -> float array array
    val kernelFromFunction :
      int -> (float -> float -> float) -> float array array
    val gabor_kernel : float -> float -> int -> float array array
    val gauss : float -> float -> float
    val apply_gabor : float Images.matrix -> int -> float array array
    val sobel_segmentation : float Images.matrix -> bool -> float array array
    val remove_with_mask :
      'a Images.matrix -> 'a Images.matrix -> 'a -> 'a array array
    val getROI : float Images.matrix -> (int * int) array
    val keepROI :
      float Images.matrix -> (int * int) array -> float array array
    val keepROI_bin :
      int Images.matrix -> (int * int) array -> int array array
    val displayROI : (int * int) array -> unit
    val testROI : float Images.matrix -> bool -> unit
    val binarization : float Images.matrix -> int -> int array array
    val getGabor : float Images.matrix -> int -> float array array
    val reverseBin : int Images.matrix -> int array array
    val p : 'a array array -> int -> int -> int -> 'a
    val img_mvt : int Images.matrix -> int array array -> unit
    val one_thining_guohall : int Images.matrix -> int -> bool
    val one_thining_zhangsuen : int Images.matrix -> int -> bool
    val thinning :
      'a Images.matrix -> ('a array array -> int -> bool) -> 'a array array
    val fullThining :
      float Images.matrix ->
      (int array array -> int -> bool) -> int -> int array array
  end =
  struct

	(* Get optimal Otsu's threshold *)
	let getOptimalThreshold_otsu m bloc_size =
		(* Create a new matrix based on bloc average values *)
		let raw_blocked_m = Images.cutInBlocs m bloc_size in
		let (h,w) = Images.getHW raw_blocked_m in
		let bloc_m_val = Array.make_matrix h w 0. in
		for i = 0 to (h-1) do
			for j = 0 to (w-1) do
				bloc_m_val.(i).(j) <- (Images.getMatrixAv raw_blocked_m.(i).(j));
			done;
		done;
		(* Get neighborhood grey gradient *)
		let bloc_m_avgrad = Array.make_matrix h w 0. in
		let cells = [|(-1, -1);(-1, 0);(-1, 1);(0, 1);(1, 1);(1, 0);(1, -1);(0, -1)|] in
		for i = 0 to (h-1) do
			for j = 0 to (w-1) do
				(* Get 8's neighborhood average *)
				let local_av = ref 0. in
				let count = ref 0. in
				for k = 0 to 7 do
					let (a,b) = cells.(k) in
					let (x,y) = (1-a,1-b) in
					if (x > 0) && (x < h) && (y > 0) && (y < w) then
						(local_av := !local_av +. bloc_m_val.(x).(y);
						count := !count +. 1.);
				done;
				(* Get gradient *)
				local_av := abs_float (bloc_m_val.(i).(j) -. !local_av); 
				(* Parse in result matrix *)
				bloc_m_avgrad.(i).(j) <- (!local_av /. !count);
			done;
		done;
		(* Get f(i,j) frequency of a couple *)
		let frequency_m = Array.make_matrix 256 256 0. in
		for k = 0 to (h-1) do
			for l = 0 to (w-1) do
				let i = int_of_float bloc_m_val.(k).(l) in
				let j = int_of_float bloc_m_avgrad.(k).(l) in
				frequency_m.(i).(j) <- (frequency_m.(i).(j) +. 1.);
			done;
		done;
		(* Calculate integral images *)
		let sN = Array.make_matrix 256 256 0. in
		let sO = Array.make_matrix 256 256 0. in
		let sG = Array.make_matrix 256 256 0. in
		let iiN = Array.make_matrix 256 256 0. in
		let iiO = Array.make_matrix 256 256 0. in
		let iiG = Array.make_matrix 256 256 0. in
		let trace = ref (-1.) in
		(* Do the calculation of sum areas *)
		for x = 0 to 255 do
			for y = 0 to 255 do
				let cur_freq = frequency_m.(x).(y) in
				let (i,j) = (float_of_int x,float_of_int y) in
				sN.(x).(y) <- if (x = 0) && (y = 0) then cur_freq
											else if (x = 0) then cur_freq +. +. sN.(x).(y-1)
											else if (y = 0) then cur_freq +. sN.(x-1).(y)
											else cur_freq +. sN.(x-1).(y) +. sN.(x).(y-1) -. sN.(x-1).(y-1);
				sO.(x).(y) <- if (x = 0) && (y = 0) then i*.cur_freq
											else if (x = 0) then i*.cur_freq +. +. sO.(x).(y-1)
											else if (y = 0) then i*.cur_freq +. sO.(x-1).(y)
											else cur_freq +. sO.(x-1).(y) +. sO.(x).(y-1) -. sO.(x-1).(y-1);
				sG.(x).(y) <- if (x = 0) && (y = 0) then j*.cur_freq
											else if (x = 0) then j*.cur_freq +. +. sG.(x).(y-1)
											else if (y = 0) then j*.cur_freq +. sG.(x-1).(y)
											else j*.cur_freq +. sG.(x-1).(y) +. sG.(x).(y-1) -. sG.(x-1).(y-1);
			done;
		done;
		(* Get sum area in rectangles *)
		for x = 0 to 255 do
			for y = 0 to 255 do
				if (x = 0) && (y = 0) then
					(iiN.(x).(y) <- sN.(x).(y);
					iiO.(x).(y) <- sO.(x).(y);
					iiG.(x).(y) <- sG.(x).(y))
				else if (x = 0) then
					(iiN.(x).(y) <- sN.(x).(y) -. sN.(x).(y-1);
					iiO.(x).(y) <- sO.(x).(y) -. sO.(x).(y-1);
					iiG.(x).(y) <- sG.(x).(y) -. sG.(x).(y-1))
				else if (y = 0) then
					(iiN.(x).(y) <- sN.(x).(y) -. sN.(x-1).(y);
					iiO.(x).(y) <- sO.(x).(y) -. sO.(x-1).(y);
					iiG.(x).(y) <- sG.(x).(y) -. sG.(x-1).(y))
				else
					(iiN.(x).(y) <- sN.(x-1).(y-1) +. sN.(x).(y) -. sN.(x).(y-1) -. sN.(x-1).(y);
					iiO.(x).(y) <- sO.(x-1).(y-1) +. sO.(x).(y) -. sO.(x).(y-1) -. sO.(x-1).(y);
					iiG.(x).(y) <- sG.(x-1).(y-1) +. sG.(x).(y) -. sG.(x).(y-1) -. sG.(x-1).(y));
			done;
		done;
		let f s t ii_in =
			ii_in.(s-1).(t-1) +. ii_in.(255).(0) -. ii_in.(255).(t-1) -. ii_in.(s-1).(0) in
		let size = float_of_int (h*w) in
		for x = 0 to 255 do
			for y = 0 to 255 do
				(* Vectors *)
				let (u00,u01) = ((iiO.(x).(y) /. iiN.(x).(y)),(iiG.(x).(y) /. iiN.(x).(y))) in
				let (u10,u11) = (((f (x+1) (y+1) iiO) /. (f (x+1) (y+1) iiN)),
												((f (x+1) (y+1) iiG) /. (f (x+1) (y+1) iiN))) in
				let (uT0,uT1) = ((iiO.(x).(y) /. size),(iiG.(x).(y) /. size)) in
				(* Probabilities *)
				let p0 = (iiN.(x).(y) /. size) in
				let p1 = ((f (x+1) (y+1) iiN) /. size) in
				(* Get trace *)
				let cur_trace = p0 *. (((u00 -. uT0)**2.) +. ((u01 -. uT1)**2.))
												+. p1 *. (((u10 -. uT0)**2.) +. ((u11 -. uT1)**2.)) in
				(* Get maximal trace *)
				if (cur_trace > !trace) then 
					trace := cur_trace;
			done;
		done;
		!trace;;

		(* Mean and variance based method of segmentation *)
		let segmentation m bloc_size threshold =
			let (h,w) = Images.getHW m in
			let ret = Array.make_matrix h w 255. in
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
						let sum = ref 0. in
						for k = !i to max_h do
							for l = !j to max_w do
								sum := !sum +. m.(k).(l);
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
			(* Remove all borders *)
			for i = 0 to h-1 do
				for j = w-bloc_size to w-1 do
					ret.(i).(j) <- 255.
				done;
			done;
			for j = 0 to w-1 do
				for i = h-bloc_size to h-1 do
					ret.(i).(j) <- 255.
				done;
			done;
		ret;;

	(* Image normalisation with histogram equalization *)
	let normalisation m =
		let (h,w) = Images.getHW m in
		(* Get image's histogramm *)
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

		(* Morphology and gradient based method of segmentation *)
		let segmentation_gradient m bloc_size threshold fft =
			let ret = Images.copyMatrix m in
			(* let m_normalised = normalisation m in *)
			let blocked_m = Images.cutInBlocs m bloc_size in
			let (h,w) = Images.getHW blocked_m in
			(* Get Gradient *)
			let gx =
				if fft then Convolution.convolve_matrix_fft Orientation.hX m
				else Convolution.convolve_matrix Orientation.hX m in
			let gy =
				if fft then Convolution.convolve_matrix_fft Orientation.hY m
				else Convolution.convolve_matrix Orientation.hY m in
			let getGrad x y i j =  (i*bloc_size+x,j*bloc_size+y) in
			for i = 0 to (h-2) do
				for j = 0 to (w-2) do
					(* Get mean *)
					let mX = ref 0. in
					let mY = ref 0. in
					for x = 0 to bloc_size do
						for y = 0 to bloc_size do
							let (a,b) = getGrad x y i j in
							mX := !mX +. gx.(a).(b);
							mY := !mY +. gy.(a).(b);
						done;
					done;
					mX := !mX /. (float_of_int (bloc_size*bloc_size));
					mY := !mY /. (float_of_int (bloc_size*bloc_size));
					(* Get standard deviations *)
					let stdx = ref 0. in
					let stdy = ref 0. in
					for x = 0 to bloc_size do
						for y = 0 to bloc_size do
							let (a,b) = getGrad x y i j in
							stdx := !mX +. (gx.(a).(b) -. !mX)**2.;
							stdy := !mY +. (gy.(a).(b) -. !mY)**2.;
						done;
					done;
					stdx := sqrt (!stdx /. (float_of_int (bloc_size*bloc_size)));
					stdy := sqrt (!stdy /. (float_of_int (bloc_size*bloc_size)));
					(* Remove the bloc if under the threshold *)
					if (!stdx +. !stdy) < threshold then
						(for x = 0 to bloc_size do
							for y = 0 to bloc_size do
								let (a,b) = getGrad x y i j in
								ret.(a).(b) <- 255.
							done;
						done;)
				done;
			done;
			(* Remove borders *)
			for i = 0 to h-1 do
				for j = w-bloc_size to w-1 do
					ret.(i).(j) <- 255.
				done;
			done;
			for j = 0 to w-1 do
				for i = h-bloc_size to h-1 do
					ret.(i).(j) <- 255.
				done;
			done;
		ret;;

	(* Kernel from function *)
	let kernelFromFunction size f =
		let kernel = Array.make_matrix size size 0. in
		for i = 0 to (size-1) do
			for j = 0 to (size-1) do
				kernel.(i).(j) <- f (float_of_int (i - size / 2)) (float_of_int (j - size / 2))
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
		let (h,w) = Images.getHW m in
		let ret = Array.make_matrix h w 0. in
		let angles = Orientation.smoothMyAngles (Orientation.getAngles m bloc_size) in
		let freqs = Frequency.frequency_map m bloc_size in
		let (h_b,w_b) = Images.getHW angles in
		let gauss_kernel = kernelFromFunction 2 gauss in
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

	(* Sobel segmentation *)
	let sobel_segmentation m fft =
		let x_cv = 
			if fft then Convolution.convolve_matrix_fft Orientation.hX m
			else Convolution.convolve_matrix Orientation.hX m in
		let y_cv =
			if fft then Convolution.convolve_matrix_fft Orientation.hY m
			else Convolution.convolve_matrix Orientation.hY m in
		let f x y = x**2. +. y**2. in
		Images.applyFunctMatrix_d x_cv y_cv f;;

	(* Remove pixels based on a mask *)
	let remove_with_mask mask orig color =
		if not ((Images.getHW mask) = (Images.getHW orig)) then
			failwith "remove_with_mask: Mask doesn't match";
		let (h,w) = Images.getHW mask in
		let ret = Array.make_matrix h w color in
		for i = 0 to (h-1) do
			for j = 0 to (w-1) do
				if not (mask.(i).(j) = color) then ret.(i).(j) <- color
				else ret.(i).(j) <- orig.(i).(j);
			done;
		done;ret;;

	(* Return ROI pixels from sobel segmented image *)
	let getROI m  =
		let (h,w) = Images.getHW m in
		let roi_size = ref 0 in
		let s_offset = 1 in (* Sobel Kernel offset *)
		(* Get ROI left side *)
		let roi_left = Array.make h (-1,-1) in
		for i = s_offset to (h-1-s_offset) do
			let j = ref s_offset in
			while (m.(i).(!j) < 10. ) && (!j < w-1-s_offset) do
				j := !j + 1;
			done;
			if (!j < w-1-s_offset) then
				(roi_left.(!roi_size)<-(i,!j+1);
				roi_size := !roi_size + 1)
		done;
		if not (!roi_size = 0) then
			(* Get ROI right side *)
			(let roi_right = Array.make (!roi_size) (-1,-1) in
			let (start_x,_) = roi_left.(0) in
			let (end_x,_) = roi_left.(!roi_size-1) in
			let roi_cur = ref 0 in
			for i = start_x to end_x do
				let j = ref (w-1-s_offset) in
				while (m.(i).(!j) < 10. ) && (!j > s_offset) do
					j := !j - 1
				done;
				if (!j > s_offset) && (!roi_cur < !roi_size) then
					(roi_right.(!roi_cur)<-(i,!j-1);
					roi_cur := !roi_cur + 1);
			done;
			(* Merge both *)
			let ret_cur = ref 0 in
			let roi = Array.make (h*w) (-1,-1) in
			for i = 0 to (!roi_size-1) do
				let (start_x,start_y) = roi_left.(i) in
				let (_,end_y) = roi_right.(i) in
				for j = start_y to end_y do
					roi.(!ret_cur)<-(start_x,j);
					ret_cur := !ret_cur + 1;
				done;
			done;
			(* Only keep relevant stuff - TODO: Please someone improve *)
			let ret = Array.make !ret_cur (-1,-1) in
			for i = 0 to (!ret_cur-1) do
				ret.(i) <- roi.(i)
			done;
			ret;)
		else [||];;

	(* Only keep ROI from a matrix *)
	let keepROI m roi =
		let (h,w) = Images.getHW m in
		let ret = Array.make_matrix h w 255.0 in
		if not ((Array.length roi) = 0) then
			(for r = 0 to ((Array.length roi)-1) do
				let (a,b) = roi.(r) in
				ret.(a).(b) <- m.(a).(b)
			done;
			ret;)
		else m;;

	(* Only keep ROI from a BIN matrix *)
	let keepROI_bin m roi =
		let (h,w) = Images.getHW m in
		let ret = Array.make_matrix h w 0 in
		if not ((Array.length roi) = 0) then
			(for r = 0 to ((Array.length roi)-1) do
				let (a,b) = roi.(r) in
				ret.(a).(b) <- m.(a).(b)
			done;
			ret;)
		else m;;

	(* Display ROI *)
	let displayROI roi =
		Graphics.set_color Graphics.red;
		for r = 0 to ((Array.length roi)-1) do
			let (a,b) = roi.(r) in
			let (x,y) = Orientation.getCircleLocation a b (Graphics.size_y ()) 1 in
			Graphics.moveto x y;
			Graphics.draw_circle x y 1;
		done;;

	(* Test ROI *)
	let testROI seg fft =
		let sobel_seg = sobel_segmentation seg fft in
		Testing.align_matrix sobel_seg;
		Testing.displayAnyMatrix sobel_seg;
		let roi = getROI sobel_seg in
		displayROI roi;;

	(* Binarization *)
	(* Classic method of local threshold with mean, often efficient after
		 contextual filtering as Gabor *)
	let binarization m bloc_size =
			let (h,w) = Images.getHW m in
			let ret = Array.make_matrix h w 0 in
			let bloc_size_sqrd = float_of_int (bloc_size*bloc_size) in
			let i = ref 0 in
			while !i < h do
				let j = ref 0 in
				while !j < w do
					(* Process calculation if not on border *)
					if not (((h-1) < (!i+bloc_size)) || ((w-1) < (!j+bloc_size))) then
						(let max_h = (!i+bloc_size)-1 in
						let max_w = (!j+bloc_size)-1 in
						(* Sum blocs intensities *)
						let sum = ref 0. in
						for k = !i to max_h do
							for l = !j to max_w do
								sum := !sum +. m.(k).(l)
							done;
						done;
						(* Get bloc mean *)
						let mean = !sum /. bloc_size_sqrd in
						(* Set pixel > mean to 1 *)
						let fullBlack = ref 0 in
						for k = !i to max_h do
							for l = !j to max_w do
								if m.(k).(l) > mean then
									(ret.(k).(l) <- 1;
									fullBlack := !fullBlack + 1);
							done;
						done;
						(* Undo previous work if bloc has been destroyed *)
						if !fullBlack > (bloc_size-2)*(bloc_size-2) then
							(for k = !i to max_h do
								for l = !j to max_w do
									ret.(k).(l) <- 0
								done;
							done;));
					j := !j + bloc_size;
				done;
				i := !i + bloc_size
			done;
		ret;;

	(* Display gabor filtered *)
	(* fingerprint2.jpg : bloc_size = 8
		 fingerprint1.jpg : bloc_size = 16
		 ppf1.png : bloc_size = 12 *)
	let getGabor matrix bloc_size =
		(* Get optimal threshold *)
		let seg_level = getOptimalThreshold_otsu matrix bloc_size in
		(* Classic segmentation *)
		let seg = segmentation matrix bloc_size seg_level in
		(* Sobel-ed Matrix *)
		let sobel_seg = sobel_segmentation seg true in (* Force FFT *)
		Testing.align_matrix sobel_seg;
		(* Get ROI *)
		let roi = getROI sobel_seg in
		(* Gabor filters *)
		let gabor = apply_gabor seg bloc_size in
		Testing.align_matrix gabor;
		(* Isolate ROI *)
		let ret = keepROI gabor roi in
		ret;;

	(* Reverse binary image *)
	let reverseBin m =
		let f a =
			if a = 1 then 0
			else 1 in
		Images.applyFunctMatrix m f;;

	(* Guo-Hall thinning algorithm - 1987 *)
	(* Get 8-neighborhood bool array *)
	let p matrix i j num =
		if num = 9 then matrix.(i-1).(j-1)
		else if num = 8 then matrix.(i).(j-1)
		else if num = 7 then matrix.(i+1).(j-1)
		else if num = 6 then matrix.(i+1).(j)
		else if num = 5 then matrix.(i+1).(j+1)
		else if num = 4 then matrix.(i).(j+1)
		else if num = 3 then matrix.(i-1).(j+1)
		else if num = 2 then matrix.(i-1).(j)
		else matrix.(i).(j) (* Fallback *) ;;

	(* Image difference *)
	(* A &= ~B in CCP *)
	let img_mvt a b =
		let (h,w) = Images.getHW a in
		for i = 0 to (h-1) do
			for j = 0 to (w-1) do
				a.(i).(j) <- ((land) a.(i).(j) (lnot b.(i).(j)));
			done;
		done;;

	(* One thining iteration *)
	let one_thining_guohall m iter =
		(* Prepare matrix *)
		let (h,w) = Images.getHW m in
		let marker = Array.make_matrix h w 0 in
		let m_bak = Images.copyMatrix m in
		let deleting = ref false in
		(* Actual loop *)
		for i = 2 to (h-2) do
			for j = 2 to (w-2) do
				(* Get values *)
				let p_cur num = p m i j num in
				let p2 = (p_cur 2) in
				let p3 = (p_cur 3) in
				let p4 = (p_cur 4) in
				let p5 = (p_cur 5) in
				let p6 = (p_cur 6) in
				let p7 = (p_cur 7) in
				let p8 = (p_cur 8) in
				let p9 = (p_cur 9) in
				(* Conditions *)
				let c  = ((land) (lnot p2) ((lor) p3 p4)) + ((land) (lnot p4) ((lor) p5 p6)) +
								 ((land) (lnot p6) ((lor) p7 p8)) + ((land) (lnot p8) ((lor) p9 p2)) in
				let n1 = ((lor) p9 p2) + ((lor) p3 p4) + ((lor) p5 p6) + ((lor) p7 p8) in
				let n2 = ((lor) p2 p3) + ((lor) p4 p5) + ((lor) p6 p7) + ((lor) p8 p9) in
				let n  = if n1 < n2 then n1 else n2 in
				let m_c  = if (iter = 0) then
								 	((land) ((lor) ((lor) p6 p7) (lnot p9)) p8)
								 else
								 	((land) ((lor) ((lor) p2 p3) (lnot p5)) p4) in
				(* Check *)
				if (c = 1 && (n >= 2 && n <= 3) && m_c = 0) then
					marker.(i).(j) <- 1;
			done;
		done;
		img_mvt m marker;
		deleting := Images.areThereNonZeros (Images.absDiff m m_bak);
		!deleting;;

	let one_thining_zhangsuen m iter =
		(* Prepare matrix *)
		let (h,w) = Images.getHW m in
		let marker = Array.make_matrix h w 0 in
		let m_bak = Images.copyMatrix m in
		let deleting = ref false in
		let bool2bin value =
			if value then 1
			else 0 in
		(* Actual loop *)
		for i = 2 to (h-2) do
			for j = 2 to (w-2) do
				(* Get values *)
				let p_cur num = p m i j num in
				let p2 = (p_cur 2) in
				let p3 = (p_cur 3) in
				let p4 = (p_cur 4) in
				let p5 = (p_cur 5) in
				let p6 = (p_cur 6) in
				let p7 = (p_cur 7) in
				let p8 = (p_cur 8) in
				let p9 = (p_cur 9) in
				(* Conditions *)
				let a = (bool2bin ((p2 = 0) && (p3 = 1))) + (bool2bin ((p3 = 0 && (p4 = 1)))) + 
								 (bool2bin ((p4 = 0) && (p5 = 1))) + (bool2bin ((p5 = 0 && (p6 = 1)))) + 
								 (bool2bin ((p6 = 0) && (p7 = 1))) + (bool2bin ((p7 = 0 && (p8 = 1)))) +
								 (bool2bin ((p8 = 0) && (p9 = 1))) + (bool2bin ((p9 = 0 && (p2 = 1)))) in
				let b = p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 in
				let m1 = if (iter = 0) then (p2 * p4 * p6) else (p2 * p4 * p8) in
				let m2 = if (iter = 0) then (p4 * p6 * p8) else (p2 * p6 * p8) in
				(* Check *)
				if ((a = 1) && ((b >= 2) && (b <= 6)) && (m1 = 0) && (m2 = 0)) then
					marker.(i).(j) <- 1;
			done;
		done;
		img_mvt m marker;
		deleting := Images.areThereNonZeros (Images.absDiff m m_bak);
		!deleting;;

	(* Actuall thinning part *)
	let thinning m methode =
		let cur_m = Images.copyMatrix m in
		(* Actual while - Add an iter check *)
		let isDeleting = ref true in
		while !isDeleting do
			isDeleting := methode cur_m 0;
			isDeleting := methode cur_m 1;
			(* Testing.displayBin cur_m; *)
		done;cur_m;;

	(* Display final result *)
	(* fingerprint2.jpg : bloc_size = 8
		 fingerprint1.jpg : bloc_size = 16
		 ppf1.png : bloc_size = 12 *)
	let fullThining matrix thin_method bloc_size =
		(* Get optimal threshold *)
		print_string "\nGet optimal threshold ...";
		let seg_level = getOptimalThreshold_otsu matrix bloc_size in
		(* Classic segmentation *)
		print_string "\nSegmentation ...";
		let seg = segmentation matrix bloc_size seg_level in
		(* Sobel-ed Matrix *)
		print_string "\nSobel Segmentation ...";
		let sobel_seg = sobel_segmentation seg  true in (* Force FFT *)
		Testing.align_matrix sobel_seg;
		(* Get ROI *)
		print_string "\nGet ROI ...";
		let roi = getROI sobel_seg in
		(* Gabor filters *)
		print_string "\nApply Gabor filters ...";
		let gabor = apply_gabor seg bloc_size in
		Testing.align_matrix gabor;
		(* Isolate ROI *)
		print_string "\nExtract ROI ...";
		let gabor_roi = keepROI gabor roi in
		(* Binarize *)
		print_string "\nBinarize Image ...";
		let bin = binarization gabor_roi bloc_size in
		(* Isolate BIN ROI *)
		print_string "\nExtract ROI from binarized image ...";
		let bin_roi = keepROI_bin bin roi in
		(* Do thinning *)
		print_string "\nApply thinning algorithm ...";
		let thin = thinning bin_roi thin_method in
		thin;;

	end
