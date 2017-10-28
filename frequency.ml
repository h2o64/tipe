(*module type FREQUENCY =
  sig

  end;;

module Frequency : FREQUENCY = *)
module Frequency =
  struct
		open Complex;;

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
		let get_signatures i j m bloc_size =
			let signature_k k = signature i j m bloc_size k in
			let signatures = Array.make (bloc_size*2) (signature_k 0) in
			for l = 1 to (bloc_size*2)-1 do
				signatures.(l)<-signature_k l;
			done;signatures;;

		(* Plot the signature *)
		let plot_signature i j m bloc_size =
			let x_factor = 15 in
			let signature_k k = signature i j m bloc_size k in
			let sign_0 = (int_of_float (signature_k 0)) in
			let prev = ref (0,sign_0) in
			let lines = Array.make (bloc_size*2) (0,0,0,sign_0) in
			let curve = Array.make (bloc_size*2) (0,sign_0,0,sign_0) in
			let max_height = ref (-1.) in
			for l = 1 to (bloc_size*2)-1 do
				let cur_sign = (signature_k l) in
				let ((a,b),(c,d)) = ((l,0),(l,(int_of_float cur_sign))) in
				let (e,f) = !prev in
				lines.(l) <- (a*x_factor,b,c*x_factor,d);
				curve.(l) <- (e*x_factor,f,c*x_factor,d);
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

		(* Print FFT results *)
		let print_fft tab =
			printf "FFT =@\n  @[";
			Array.iteri (fun i yi -> printf "[%d] %f %+fi@\n" i yi.re yi.im) tab;;
			printf "@]@\n";;

		(* Get signature's fourrier transform *)
		let fft_signature i j m bloc_size offset limit_height =
			let signatures = get_signatures i j m bloc_size in
			let complex_signatures = FFT.complex_array_of_array signatures in
			let fft_signatures = FFT.fft complex_signatures in
			let x_factor = 15 in
			let lines = Array.make (bloc_size*2) (0,0,0,0) in
			let pic = ref (-1,-1.) in
			for l = 1 to (bloc_size*2)-1 do
				let norme = norm fft_signatures.(l) in
				let (_,a) = !pic in
				if a < norme then pic := (l,norme);
				lines.(l)<-(offset+l*x_factor,0,offset+l*x_factor,int_of_float norme)
			done;
			let (a,max_height) = !pic in
			(* Adjust height to the limit *)
			let y_factor =
				let tmp = (int_of_float max_height)/limit_height in
				if tmp > 0 then tmp
				else 1 in
			for l = 1 to (bloc_size*2)-1 do
				let (x0,y0,x1,y1) = lines.(l) in
				lines.(l)<-(x0,y0/y_factor,x1,y1/y_factor);
			done;
			print_string "\nRidge Frequency = ";
			print_int a;
			print_string "\n";
			set_line_width 2;
			set_color red;
			(* Clear graph *)
			clear_graph ();
			draw_segments lines;
			!pic;;

		(* Interactive signature *)
		let interactive_signature img bloc_size =
			let x_factor = 15 in
			let grey_im = Images.imageToGreyScale img in
			let width = img.width+((bloc_size*2*x_factor)-1) in
			open_graph (Images.getFormat width img.height);
			auto_synchronize true;
			set_line_width 4;
			while true do
				let (i,j) = (mouse_pos ()) in
				(* Clear graph *)
				draw_image (make_image img.matrix) 0 0;
				(* Print mouse co *)
				print_string "(i,j) = ";
				print_int i;
				print_string ";";
				print_int j;
				(* Print pointer location *)
				draw_circle i j (bloc_size/2);
				(* Display FFT *)
				let (a,_) = fft_signature i j grey_im.matrix bloc_size img.width img.height in
				(* Display ridge frequency *)
				let x_text = (((bloc_size*2*x_factor)-1)/2 + img.width) in
				let y_text = (int_of_float ((3./.4.) *. (float_of_int img.height))) in
				moveto x_text y_text;
				draw_string (String.concat "" ["Ridge Frequency = ";string_of_int a]);
			done;;

	end
