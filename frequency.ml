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

		(* Plot the signature *)
		let plot_fft_signature i j m bloc_size =
			let fft_signatures = fft_signature i j m bloc_size in
			let int_fft_signatures = Plot.int_of_float_array fft_signatures in
			close_graph ();
			open_graph " 1x1";
			Plot.plot_array (int_fft_signatures,0,15,true,red,"FFT Signature",4,true,0,false,true);;

		(* Interactive signature *)
		let interactive_signature img bloc_size =
			let grey_im = Images.imageToGreyScale img in
			let fft_signatures = fft_signature 100 100 grey_im.matrix bloc_size in
			let int_fft_signatures = Plot.int_of_float_array fft_signatures in
			close_graph (); (* Close any existing graph *)
			open_graph (Images.getFormat img.width img.height);
			draw_image (make_image img.matrix) 0 0;
			Plot.plot_array (int_fft_signatures,img.width,15,true,red,"FFT Signature",4,true,0,true,true);
	    let _ = read_key() in
				while true do
					let (i,j) = Plot.plot_mouse bloc_size red 4 in
					let fft_signatures = fft_signature i j grey_im.matrix bloc_size in
					let int_fft_signatures = Plot.int_of_float_array fft_signatures in
					clear_graph (); (* Clear *)
					draw_image (make_image img.matrix) 0 0;
					Plot.plot_array (int_fft_signatures,img.width,15,true,red,"FFT Signature",4,false,0,true,true);
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
