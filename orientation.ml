module type ORIENTATION =
	sig
		val pi : float
		val hY : float Images.matrix
		val hX : float Images.matrix
		val getAngles : float Images.matrix -> int -> float Images.matrix
		val double_int_of_float : float * float -> int * int
		val getCircleLocation : int -> int -> int -> int -> int * int
		val getStartEndLine : int -> int -> int -> float -> (int * int) * (int * int)
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

		(* Convert a double of float to a double of int *)
		let double_int_of_float (a,b) = (int_of_float a,int_of_float b);;

		(* Get circle location *)
		let getCircleLocation i j h bloc_size =
			let (x,y) = ((i*bloc_size+(bloc_size/2)),(j*bloc_size+(bloc_size/2))) in
			((y),(h - x));;

		(* Get line coordonates *)
		let getStartEndLine x y bloc_size tang =
			let w = float_of_int bloc_size in
			let h_w = (w/.2.) in
			let i = float_of_int x in
			let j = float_of_int y in
			if (-1. <= tang) && (tang <= 1.) then
				let a = (i, (-1.) *. h_w *. tang +. j +. h_w) in
				let b = (i +. w, h_w *. tang +. j +. h_w) in
				((double_int_of_float a),(double_int_of_float b));
			else
				let a = (i +. h_w +. w/.(2. *. tang), j +. h_w) in
				let b = (i +. h_w -. w/.(2. *. tang), j -. h_w) in
				((double_int_of_float a),(double_int_of_float b));;

		(* Display vector field *)
		let vector_field img bloc_size =
			let grey_im = Images.imageToGreyScale img in
			let angles = getAngles grey_im.matrix (bloc_size/4) in
			open_graph (Images.getFormat img.width img.height);
			set_line_width 1;
			set_color red;
			draw_image (make_image img.matrix) 0 0;
			let i = ref 1 in
			while !i < grey_im.height do
				let j = ref 1 in
				while !j < grey_im.width do
					let tang = tan angles.(!i-1/(bloc_size/4)).(!j-1/(bloc_size/4)) in
					let ((x0,y0),(x1,y1)) = (getStartEndLine !i !j (bloc_size/4) tang) in
					let (a,b) = getCircleLocation x0 y0 grey_im.height (bloc_size/4) in
					let (c,d) = getCircleLocation x1 y1 grey_im.height (bloc_size/4) in
					draw_segments [|(a,b,c,d)|];
					j := !j + (bloc_size/4);
				done;
				i := !i + (bloc_size/4);
			done;;
	end
