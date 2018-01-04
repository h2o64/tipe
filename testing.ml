module type TESTING =
  sig
		val test_image : Graphics.color Images.image
		val filter_test : Graphics.color Images.image -> float Images.matrix -> unit
		val blockedToFull : int -> int -> int -> float Images.matrix -> float Images.matrix
		val displayAnyMatrix : float Images.matrix -> unit
		val dbg_int : string -> int -> bool -> unit
		val dbg_float : string -> float -> bool -> unit
		val get_array_max : 'a array -> 'a
		val get_matrix_max : 'a array array -> 'a
		val align_matrix : float array array -> unit
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
			let (h,w) = Images.getHW matrix in
			let last = Images.matrixApply Images.rgb_of_greyscale matrix in
			open_graph (Images.getFormat w h);
			dessiner_image last;;

		let dbg_int text value jump =
			if jump then print_string "\n";
			print_string "[DBG] ";
			print_string text;
			print_string " = ";
			print_int value;;

		let dbg_float text value jump =
			if jump then print_string "\n";
			print_string "[DBG] ";
			print_string text;
			print_string " = ";
			print_float value;;

		let get_array_max tab =
			let ret = ref tab.(0) in
			for i = 1 to ((Array.length tab)-1) do
				if tab.(i) > !ret then ret := tab.(i);
			done;!ret;;

		let get_matrix_max m =
			let ret = ref (get_array_max m.(0)) in
			for i = 1 to ((Array.length m)-1) do
				let tmp = (get_array_max m.(i)) in
				if tmp > !ret then ret := tmp;
			done;!ret;;

		let align_matrix m =
			let (h,w) = Images.getHW m in
			let matrix_max = get_matrix_max m in
			let convert num = (num *. 255.) /. matrix_max in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					m.(i).(j) <- convert m.(i).(j);
				done;
			done;;

	end
