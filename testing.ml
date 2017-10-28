module type TESTING =
  sig
		val test_image : Graphics.color Images.image
		val filter_test : Graphics.color Images.image -> float Images.matrix -> unit
		val blockedToFull : int -> int -> int -> float Images.matrix -> float Images.matrix
		val displayAnyMatrix : float Images.matrix -> unit
  end;;

module Testing : TESTING = 
  struct
		(* Open image to analyse *)
		let test_image = Images.import_image "Images/fingerprint2.jpg"

		let filter_test image kernel =
			let bw_img = Images.imageToGreyScale image in
			let m = Convolution.applyFilter bw_img.matrix kernel in
			let last = Images.matrixApply Images.rgb_of_greyscale m in
			open_graph (Images.getFormat image.width image.height);
			dessiner_image last;;

		let blockedToFull h w bloc_size matrix =
			let ret = Array.make_matrix h w 255. in
			for i = 0 to ((h - 1) - (h mod bloc_size)) do
				for j = 0 to ((w - 1) - (w mod bloc_size)) do
					ret.(j).(j) <- matrix.(i/bloc_size).(j/bloc_size)
				done;
			done;ret;;

		let displayAnyMatrix matrix =
			let (h,w) = ((Array.length matrix),(Array.length matrix.(0))) in
			let last = Images.matrixApply Images.rgb_of_greyscale matrix in
			open_graph (Images.getFormat w h);
			dessiner_image last;;
	end
