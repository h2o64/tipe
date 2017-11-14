module type FREQUENCY =
  sig
    val signature : int -> int -> float array array -> int -> int -> float
    val get_signatures :
      int -> int -> float array array -> int -> float array
    val plot_signature : int -> int -> float array array -> int -> unit
    val print_fft : Complex.t array -> unit
    val removeDC : float array -> unit
    val fft_signature : int -> int -> float array array -> int -> float array
    val plot_fft_signature : int -> int -> float array array -> int -> unit
    val interactive_signature : Graphics.color Images.image -> int -> unit
    val pp_int_pair : int * float -> unit
    val getFrequency : 'a array -> float
    val isvalid : float array -> float -> bool
    val w_barre : float array -> float
    val frequency_interpolation :
      int -> int -> float Images.matrix -> int -> float
    val frequency_image : Graphics.color Images.image -> int -> unit
  end;;

module Frequency : FREQUENCY =
  struct
		open Complex;;

		(* Get the signature of the windows (bloc_size,2*bloc_size) centered at (i,j) *)
		let signature i j m bloc_size k =
			let (he,wi) = ((Array.length m),(Array.length m.(0))) in
			let x = ref 0. in
			let w = float_of_int bloc_size in
			let angles = Orientation.getAngles_vector m bloc_size in
			let angle = angles.((i-1)/bloc_size).((j-1)/bloc_size) in
			let (y,z) = (float_of_int i,float_of_int j) in
			for d = 0 to bloc_size-1 do
				let u = y +. ((float_of_int d) -. (w/.2.)) *. (cos angle) +. ((float_of_int k) -. 1./.2.) *. (sin angle) in
				let v = z +. ((float_of_int d) -. (w/.2.)) *. (sin angle) -. ((float_of_int k) -. 1./.2.) *. (cos angle) in
				let u_int = (int_of_float u) in
				let v_int = (int_of_float v) in
				if (u_int < he) && (v_int < wi) && (u_int > 0) && (v_int > 0) then
					x := !x +. m.(u_int).(v_int);
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
			Plot.plot_array (int_signatures,0,15,false,green,"Signature",4,true,0,false,false);;

		(* Print FFT results *)
		let print_fft tab =
			printf "FFT =@\n  @[";
			Array.iteri (fun i yi -> printf "[%d] %f %+fi@\n" i yi.re yi.im) tab;;
			printf "@]@\n";;

		(* Remove average value from an array *)
		let removeDC tab =
			let n = Array.length tab in
			let av = ref (0.) in
			for i = 0 to (n-1) do
				av := tab.(i) +. !av
			done;
			av := (!av /. (float_of_int n));
			for j = 0 to (n-1) do
				tab.(j)<-(tab.(j) -. !av);
			done;;

		(* Get signature's fourrier transform *)
		let fft_signature i j m bloc_size =
			let signatures = get_signatures i j m bloc_size in
			removeDC signatures;
			let complex_signatures = FFT.complex_array_of_array signatures in
			let fft_signatures = FFT.fft complex_signatures in
			let ret = Array.make (bloc_size*2) 0. in
			for l = 0 to (bloc_size*2)-1 do
				let norme = (norm fft_signatures.(l)) in ret.(l)<-norme
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
			let maxi = ref (1,spectrum.(0)) in
			for i = 1 to ((Array.length spectrum)-1) do
				let (x,y) = !maxi in
				if spectrum.(i) > y then (maxi := (i+1,spectrum.(i)));
			done;
			let (x,y) = !maxi in (1./.(float_of_int x));;

		(* Check spectrum's validity - If there's peaks *)
		let isvalid spectrum b =
			let count = ref 0 in
			let i = ref 1 in
			let records = Array.make 2 (-1,-1.) in
			while not (!count = 2) do
				print_string "\nI'M IN A WHILE\n";
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
			for j = 0 to ((Array.length spectrum)-1) do
				if spectrum.(j) *. b >= (float_of_int a) then ret := true
			done;
			!ret;;

		(* Get W(i,j) from spectrum of w(i,j) window *)
		let w_barre spectrum =
			let freq = getFrequency spectrum in
			(* if (freq < (1./.3.)) || (freq > (1./.25.)) then -1.
			else *) if isvalid spectrum 2. then 1.
			else freq;;

		(* Frequency interpolation *)
		let frequency_interpolation i j m bloc_size =
			let mu x = if x < 0. then 0. else x in
			let sigma x = if x < 0. then 0. else float_of_int (bloc_size*2) in
			let spect = ref (fft_signature i j m bloc_size) in
			let w_cur = ref (w_barre !spect) in
			if not(!w_cur = -1.) then !w_cur
			else
				(let num = ref 0. in
				let dem = ref 0. in
				for u = -3 to 3 do
					for v = -3 to 3 do
						print_string "\nu = ";
						print_int u;
						print_string " | v = ";
						print_int v;
						let gaussian = Convolution.convolve i j Convolution.gaussian_special_kernel m in
						print_string " | STEP1";
						spect := (fft_signature (i - u*bloc_size) (j - v*bloc_size) m bloc_size);
						print_string " | STEP2";
						w_cur := (w_barre !spect);
						print_string " | STEP3 | ";
						num := !num +. gaussian *. (mu !w_cur);
						dem := !dem +. gaussian *. (sigma !w_cur+.1.);
						print_float !w_cur;
					done;
				done;
				(!num /. !dem));;

	(* Frequency image *)
	let frequency_image img bloc_size =
			let grey_im = Images.imageToGreyScale img in
			let (h,w) = (img.width,img.height) in
			let ret_image = Array.make_matrix h w 0. in
			close_graph (); (* Close any existing graph *)
			open_graph (Images.getFormat h w);
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					print_string "\ni = ";
					print_int i;
					print_string " | j = ";
					print_int j;
					let spec = fft_signature i j grey_im.matrix bloc_size in
					let freq = getFrequency spec in
					if (freq >= 1./.3.) && (freq <= 1./.25.) then
						(let intensity = (1./.freq) *. 10.2 in ret_image.(i).(j) <- intensity);
				done;
			done;(Testing.displayAnyMatrix ret_image);;

	end
