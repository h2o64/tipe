(* module type IMAGE_PROCESSING =
  sig
    val segmentation :
      float Images.matrix -> int -> float -> float array array
    val normalisation : float Images.matrix -> float array array
    val kernelFromFunction :
      int -> (float -> float -> float) -> float array array
    val gabor_kernel : float -> float -> int -> float array array
    val gauss : float -> float -> float
    val apply_gabor : float Images.matrix -> int -> float array array
    val sobel_segmentation : float Images.matrix -> float array array
    val do_everything : float Images.matrix -> int -> float -> bool -> unit
  end;;

module Image_Processing : IMAGE_PROCESSING = *)
module Image_Processing =
  struct
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
		let xsigma_sqrd = 25. in
		let ysigma_sqrd = 25. in
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
	let sobel_segmentation m =
		let x_cv = Convolution.convolve_matrix Orientation.hX m in
		let y_cv = Convolution.convolve_matrix Orientation.hY m in
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
		(* Get ROI left side *)
		let roi_left = Array.make h (-1,-1) in
		for i = 1 to (h-2) do
			let j = ref 1 in
			while (m.(i).(!j) = 0.) && (!j < w-2) do
				j := !j + 1;
			done;
			if (!j < w-2) then
				(roi_left.(!roi_size)<-(i,!j+1);
				roi_size := !roi_size + 1)
		done;
		(* Get ROI right side *)
		let roi_right = Array.make (!roi_size) (-1,-1) in
		let (start_x,_) = roi_left.(0) in
		let (end_x,_) = roi_left.(!roi_size-1) in
		let i = ref start_x in
		let roi_cur = ref 0 in
		while (!i <= end_x) do
			let j = ref (w-2) in
			while (m.(!i).(!j) = 0.) do
				j := !j - 1
			done;
			roi_right.(!roi_cur)<-(!i,!j-1);
			roi_cur := !roi_cur + 1;
			i := !i + 1
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
		ret;;

	(* Only keep ROI from a matrix *)
	let keepROI m roi =
		let (h,w) = Images.getHW m in
		let ret = Array.make_matrix h w 255.0 in
		for r = 0 to ((Array.length roi)-1) do
			let (a,b) = roi.(r) in
			ret.(a).(b) <- m.(a).(b)
		done;ret;;

	(* Only keep ROI from a BIN matrix *)
	let keepROI_bin m roi =
		let (h,w) = Images.getHW m in
		let ret = Array.make_matrix h w 0 in
		for r = 0 to ((Array.length roi)-1) do
			let (a,b) = roi.(r) in
			ret.(a).(b) <- m.(a).(b)
		done;ret;;

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
						for k = !i to max_h do
							for l = !j to max_w do
								if m.(k).(l) > mean then ret.(k).(l) <- 1
							done;
						done;);
					j := !j + bloc_size;
				done;
				i := !i + bloc_size
			done;
		ret;;

	(* Display gabor filtered *)
	(* fingerprint2.jpg : bloc_size = 8 | seg_level = 4100
		 fingerprint1.jpg : bloc_size = 16 | seg_level = 4100
		 ppf1.png : bloc_size = 12 | seg_level = 400 *)
	let getGabor matrix bloc_size seg_level norm useROI =
		(* Classic segmentation *)
		let seg = ref (segmentation matrix bloc_size seg_level) in
		(* Sobel-ed Matrix *)
		let sobel_seg = sobel_segmentation !seg in
		(* Get ROI *)
		let roi = getROI sobel_seg in
		(* Normalisation *)
		if norm then seg := (normalisation !seg);
		(* Gabor filters *)
		let gabor = apply_gabor !seg bloc_size in
		Testing.align_matrix gabor;
		(* Isolate ROI *)
		let ret = ref gabor in
		if useROI then ret := keepROI gabor roi;
		!ret;;

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
		else if num = 1 then matrix.(i).(j)
		else matrix.(i).(j) (* Fallback *) ;;

	(* Work with a boolean matrix *)
	let bin2bool value = (value = 1);;
	let bool2bin value =
		if value then 1
		else 0;;

	(* Image difference *)
	(* A &= ~B in CCP *)
	let img_mvt a b =
		let (h,w) = Images.getHW a in
		for i = 0 to (h-1) do
			for j = 0 to (w-1) do
				let a_b = bin2bool a.(i).(j) in
				let b_b = bin2bool b.(i).(j) in
				a.(i).(j) <- bool2bin (a_b && (not b_b));
			done;
		done;;

	(* One thining iteration *)
	let one_thining m iter =
		(* Prepare matrix *)
		let (h,w) = Images.getHW m in
		let marker = Array.make_matrix h w 0 in
		let m_bak = Images.copyMatrix m in
		let m_b = Images.applyFunctMatrix m bin2bool in
		let deleting = ref false in
		(* C(P1)   = !P2 & (P3 | P4) + !P4 & (P5 | P6) + !P6 & (P7 | P8) + !P8 & (P9 | P2) *)
		let c matrix i j =
			let p_cur = p matrix i j in
			let a = ((not (p_cur 2)) && ((p_cur 3) || p_cur 4)) in
			let b = ((not (p_cur 4)) && ((p_cur 5) || p_cur 6)) in
			let c = ((not (p_cur 6)) && ((p_cur 7) || p_cur 8)) in
			let d = ((not (p_cur 8)) && ((p_cur 9) || p_cur 2)) in
			((bool2bin a) + (bool2bin b) + (bool2bin c) + (bool2bin d)) in
		(* N1(P1) = (P9 | P2) + (P3 | P4) + (P5 | P6) + (P7 | P8) *)
		let n1 matrix i j =
			let p_cur = p matrix i j in
			bool2bin (p_cur 9 || p_cur 2) +
			bool2bin (p_cur 3 || p_cur 4) +
			bool2bin (p_cur 5 || p_cur 6) +
			bool2bin (p_cur 7 || p_cur 8) in
		(* N2(P1) = (P2 | P3) + (P4 | P5) + (P6 | P7) + (P8 | P9) *)
		let n2 matrix i j =
			let p_cur = p matrix i j in
			bool2bin (p_cur 2 || p_cur 3) +
			bool2bin (p_cur 4 || p_cur 5) +
			bool2bin (p_cur 6 || p_cur 7) +
			bool2bin (p_cur 8 || p_cur 9) in
		(* N(P1)   = MIN[N1(P1), N2(P1)] *)
		let n matrix i j = min (n1 matrix i j) (n2 matrix i j) in
		(* M(P1) =
				* if iter = 0 : ((P6 | P7 | !P9) & P8)
				* if iter = 1 : ((P2 | P3 | !P5) & P4) *)
		let m_f matrix i j =
			let p_cur = p matrix i j in
			if (iter = 1) then
				(((p_cur 2) || (p_cur 3) || (not (p_cur 5))) && (p_cur 4))
			else
				(((p_cur 6) || (p_cur 7) || (not (p_cur 9))) && (p_cur 8)); in
		(* Actual loop *)
		for i = 2 to (h-2) do
			for j = 2 to (w-2) do
				let cond1 = ((c m_b i j) = 1) in
				let cond2 = (2 <= (n m_b i j)) && ((n m_b i j) <= 3) in
				let cond3 = ((m_f m_b i j) = false) in
				if (cond1 && cond2 && cond3) then
					marker.(i).(j) <- 1;
			done;
		done;
		img_mvt m marker;
		deleting := Images.areThereNonZeros(Images.absDiff m m_bak);
		!deleting;;

	(* Actuall thinning part *)
	let thinning m =
		let cur_m = Images.copyMatrix m in
		(* Actual while - Add an iter check *)
		let isDeleting = ref true in
		while !isDeleting do
			isDeleting := one_thining cur_m 0;
			isDeleting := one_thining cur_m 1;
			Testing.displayBin cur_m;
		done;cur_m;;

	(* Display final result *)
	(* fingerprint2.jpg : bloc_size = 8 | seg_level = 4100
		 fingerprint1.jpg : bloc_size = 16 | seg_level = 4100
		 ppf1.png : bloc_size = 12 | seg_level = 400 *)
	let fullThining matrix bloc_size seg_level norm =
		(* Classic segmentation *)
		print_string "\nSegmentation ...";
		let seg = ref (segmentation matrix bloc_size seg_level) in
		(* Sobel-ed Matrix *)
		print_string "\nSobel Segmentation ...";
		let sobel_seg = sobel_segmentation !seg in
		(* Get ROI *)
		print_string "\nGet ROI ...";
		let roi = getROI sobel_seg in
		(* Normalisation *)
		if norm then
			(print_string "\nNormalisation ...";
			seg := (normalisation !seg));
		(* Gabor filters *)
		print_string "\nApply Gabor filters ...";
		let gabor = apply_gabor !seg bloc_size in
		Testing.align_matrix gabor;
		(* Isolate ROI *)
		print_string "\nExtract ROI ...";
		let gabor_roi = keepROI gabor roi in
		(* Binarize *)
		print_string "\nBinarize Image ...";
		let bin = binarization gabor_roi 12 in
		(* Isolate BIN ROI *)
		print_string "\nExtract ROI from binarized image ...";
		let bin_roi = keepROI_bin bin roi in
		(* Do thinning *)
		print_string "\nApply thinning algorithm ...";
		let thin = thinning bin_roi in
		thin;;

	end
