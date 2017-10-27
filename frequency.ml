(*module type FREQUENCY =
  sig

  end;;

module Frequency : FREQUENCY = *)
module Frequency =
  struct
		(* Get the signature of the windows (bloc_size,2*bloc_size) centered at (i,j) *)
		let signature i j m bloc_size k =
			let x = ref 0. in
			let w = float_of_int bloc_size in
			let angles = Orientation.getAngles_vector m bloc_size in
			let angle = angles.((i-1)/bloc_size).((j-1)/bloc_size) in
			let (y,z) = (float_of_int i,float_of_int j) in
			for d = 0 to bloc_size-1 do
				let u = y +. ((float_of_int d) -. (w/.2.)) *. (cos angle) +. ((float_of_int k) -. 1./.2.) in
				let v = z +. ((float_of_int d) -. (w/.2.)) *. (sin angle) -. ((float_of_int k) -. 1./.2.) in
				x := !x +. m.(int_of_float u).(int_of_float v);
			done;
			(!x/.w);;

		(* Plot the signature *)
		let plot_signature i j m bloc_size =
			let x_factor = 15 in
			let signature_k k = signature i j m bloc_size k in
			let sign_0 = (int_of_float (signature_k 0)) in
			let prev = ref (0,sign_0) in
			let lines = Array.make (bloc_size*2) (0,0,0,sign_0) in
			let curve = Array.make (bloc_size*2) (0,sign_0,0,sign_0) in
			let max_height = ref (-1.) in
			for i = 1 to (bloc_size*2)-1 do
				let cur_sign = (signature_k i) in
				let ((a,b),(c,d)) = ((i,0),(i,(int_of_float cur_sign))) in
				let (e,f) = !prev in
				lines.(i) <- (a*x_factor,b,c*x_factor,d);
				curve.(i) <- (e*x_factor,f,c*x_factor,d);
				prev := (c,d);
				if cur_sign > !max_height then max_height := cur_sign;
			done;
			open_graph (Images.getFormat ((bloc_size*2*15)-1) (int_of_float (!max_height *. 1.5)));
			set_line_width 2;
			set_color red;
			draw_segments lines;
			set_line_width 3;
			set_color green;
			draw_segments curve;;
	end
