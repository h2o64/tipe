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
				let u = y +. ((float_of_int d) -. (w/.2.)) *. (cos angle) +. ((float_of_int k) -. 1./.2.) *. (sin angle) in
				let v = z +. ((float_of_int d) -. (w/.2.)) *. (sin angle) -. ((float_of_int k) -. 1./.2.) *. (cos angle) in
				x := !x +. m.(int_of_float u).(int_of_float v);
			done;
			(!x/.w);;

		(* Get the signature *)
		let get_signatures i j m bloc_size =
			let signature_k k = signature i j m bloc_size k in
			let signatures = Array.make (bloc_size*2) (signature_k 0) in
			for l = 0 to (bloc_size*2)-1 do
				signatures.(l)<-signature_k l;
			done;signatures;;

		(* Plot the signature *)
		let plot_signature i j m bloc_size =
			let signatures = get_signatures i j m bloc_size in
			let int_signatures = Plot.int_of_float_array signatures in
			close_graph ();
			open_graph " 1x1";
			Plot.plot_array (int_signatures,0,15,true,red,"Signature",4,true,0,false,true);
			Plot.plot_array (int_signatures,1,15,false,green,"Signature",4,false,0,false,false);;

		(* Print FFT results *)
		let print_fft tab =
			printf "FFT =@\n  @[";
			Array.iteri (fun i yi -> printf "[%d] %f %+fi@\n" i yi.re yi.im) tab;;
			printf "@]@\n";;

		(* Get signature's fourrier transform *)
		(* NOTE: The 1st FFT's element is the DC, so ignored *)
		let fft_signature i j m bloc_size =
			let signatures = get_signatures i j m bloc_size in
			let complex_signatures = FFT.complex_array_of_array signatures in
			let fft_signatures = FFT.fft complex_signatures in
			let ret = Array.make (bloc_size*2) 0. in
			for l = 1 to (bloc_size*2)-1 do
				let norme = norm fft_signatures.(l) in ret.(l)<-norme
			done;ret;;

		let plot_fft_signature i j m bloc_size offset limit_height =
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
				let (a,_) = plot_fft_signature i j grey_im.matrix bloc_size img.width img.height in
				(* Display ridge frequency *)
				let x_text = (((bloc_size*2*x_factor)-1)/2 + img.width) in
				let y_text = (int_of_float ((3./.4.) *. (float_of_int img.height))) in
				moveto x_text y_text;
				draw_string (String.concat "" ["Ridge Frequency = ";string_of_int a]);
			done;;

		(* Tuple printer *)
		let pp_int_pair (x,y) =
			printf "(%d,%f)" x y;;

		(* Get ridge frequency of a power spectrum *)
		let getFrequency spectrum =
			let maxi = ref (-1,-1.) in
			for i = 1 to ((Array.length spectrum)-1) do
				let (x,y) = !maxi in
				if spectrum.(i) > y then (maxi := (i,spectrum.(i)));
			done;
			let (x,y) = !maxi in (1./.(float_of_int x));;

		(* Check spectrum's validity - If there's peaks *)
		let isvalid spectrum b =
			let count = ref 0 in
			let i = ref 1 in
			let records = Array.make 2 (-1,-1.) in
			while not (!count = 2) do
				let (a,b) = records.(!count) in
				if spectrum.(!i) > b then
					(records.(!count) <- (!i,spectrum.(!i));
					spectrum.(!i) <- (-1.);
					count := !count+1;
					i := 1)
				else i := !i + 1
			done;
			let ret = ref false in
			let (a,_) = records.(1) in
			for j = 1 to ((Array.length spectrum)-1) do
				if spectrum.(j) *. b >= (float_of_int a) then ret := true
			done;
			!ret;;

		(* Get W(i,j) from spectrum of w(i,j) window *)
		let w_barre spectrum =
			let freq = getFrequency spectrum in
			if (freq < (1./.3.)) || (freq > (1./.25.)) then -1.
			else if isvalid spectrum 2. then 1.
			else freq;;

		(* Frequency interpolation *)
		let frequency_interpolation i j m bloc_size =
			let mu x = if x < 0. then 0. else x in
			let sigma x = if x < 0. then 0. else float_of_int (bloc_size*2) in
			let spect = ref (fft_signature i j m bloc_size) in
			let w_cur = ref (w_barre !spect) in
			if !w_cur > 0. then !w_cur
			else
				(let num = ref 0. in
				let dem = ref 0. in
				for u = -3 to 3 do
					for v = -3 to 3 do
						let gaussian = Convolution.convolve i j Convolution.gaussian_special_kernel m in
						spect := (fft_signature (i - u*bloc_size) (j - v*bloc_size) m bloc_size);
						w_cur := (w_barre !spect);
						num := !num +. gaussian *. (mu !w_cur);
						dem := !dem +. gaussian *. (sigma !w_cur+.1.);
						print_float !num;
					done;
				done;
				(!num /. !dem));
	end
