module type ORIENTATION =
  sig
		val pi : float
		val hX : float Images.matrix
		val hY : float Images.matrix
		val getAngles : float Images.matrix -> int -> float Images.matrix
		val getCircleLocation : int -> int -> int -> int -> int * int
		val vector_field : Graphics.color Images.image -> int -> unit
  end;;

module Orientation : ORIENTATION = 
  struct
		(* Uses Sobel operator *)
		(* https://en.wikipedia.org/wiki/Prewitt_operator *)
		(* https://en.wikipedia.org/wiki/Sobel_operator *)
		let pi = 4. *. atan 1.
		 (* Sobel *)
		(* let (hX : float Images.matrix) = [|[|1.;0.;-1.|];[|2.;0.;-2.|];[|1.;0.;-1.|]|];;
		let (hY : float Images.matrix) = [|[|1.;2.;1.|];[|0.;0.;0.|];[|-1.;-2.;-1.|]|];; *)
		let (hY : float Images.matrix) = [|[|-1.;0.;1.|];[|-2.;0.;2.|];[|-1.;0.;1.|]|];;
		let (hX : float Images.matrix) = [|[|-1.;-2.;-1.|];[|0.;0.;0.|];[|1.;2.;1.|]|];;
		(* Based of Kass and Witkin (1987) researches *)
		let getAngles m bloc_size =
			let (h,w) = ((Array.length m),(Array.length m.(0))) in
			let (h_new,w_new) = (h-1/bloc_size,w-1/bloc_size) in
			let ret = Array.make_matrix h_new w_new 0. in
			let i = ref 1 in
			while !i < h do
				let j = ref 1 in
				while !j < w do
					let num = ref 0. in
					let dem = ref 0. in
					for k = !i to (min (!i+bloc_size) (h-1)) do
						for l = !j to (min (!j+bloc_size) (w-1)) do
							let tmp_x = Convolution.convolve k l hX m in
							let tmp_y = Convolution.convolve k l hY m in
							(num := !num +. (2. *. tmp_x *. tmp_y);
							dem := !dem +. (tmp_x**2. -. tmp_y**2.));
						done;
					done;
					let angle = (pi +. (atan2 !num !dem))/.2. in
					ret.((!i-1)/bloc_size).((!j-1)/bloc_size) <- angle;
					j := !j + bloc_size;
				done;
				i := !i + bloc_size;
			done;ret;;

		(* Get line coordonates *)
		let getStartEndLine x y teta bloc_size =
			(* Descrete values : multiples of 45° *)
			let deg_of_rad x = int_of_float ((x*.180.)/.pi) in
			let deg = (abs (deg_of_rad teta))/45 in
			let w = bloc_size/2 in
			print_string "\ndeg = ";
			print_int (deg*45);
			if ((deg mod 4) = 0) then (x,y+w,x,y-w)
			else if ((deg mod 4) = 1) then (x+w,y+w,x-w,y-w)
			else if ((deg mod 4) = 2) then (x-w,y,x+w,y)
			else if ((deg mod 4) = 3) then (x-w,y+w,x+w,y-w)
			else (0,0,0,0);;

		(* Get circle location *)
		let getCircleLocation i j h bloc_size =
			let (x,y) = ((i*bloc_size+(bloc_size/2)),(j*bloc_size+(bloc_size/2))) in
			((y),(h - x));;

		(* Display vector field *)
		let vector_field img bloc_size =
			let grey_im = Images.imageToGreyScale img in
			let angles = getAngles grey_im.matrix bloc_size in
			set_line_width 2;
			set_color red;
			draw_image (make_image img.matrix) 0 0;
			for i = 0 to ((Array.length angles)-1) do
				for j = 0 to ((Array.length angles.(0))-1) do
					let (x,y) = getCircleLocation i j img.height bloc_size in
					let (x1,y1,x2,y2) = (getStartEndLine x y angles.(i).(j) bloc_size) in
					draw_segments [|x1,y1,x2,x2|];
				done;
			done;;
	end
