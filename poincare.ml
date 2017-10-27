module type POINCARE =
  sig
		val pi : float
		type sp = { mutable x : int; mutable y : int; mutable typ : int; }
		val getAngleBetween : float -> float -> float
		val cells : (int * int) array
		val array_of_matrix : float Images.matrix -> float array
		val allowance : int -> float
		val sumAngles : int -> int -> float array array -> int -> sp
		val poincare_index : float Images.matrix -> int -> int -> sp array array
		val display_sp : Graphics.color Images.image -> int -> int -> unit
  end;;

module Poincare : POINCARE =
  struct
		let pi = 4. *. atan 1.
		(* 0 = loop | 1 = delta | 2 = whorl | 3 = nothing *)
		type sp = { mutable x : int ; mutable y : int ; mutable typ : int };;

		(* Get the angle between two angles *)
		let getAngleBetween x y = pi -. abs_float (abs_float ((x -. y) -. pi));;

		(* Make a array from a matrix *)
		(* NOTE: Only 3x3 *)
		let cells = [|(-1, -1);(-1, 0);(-1, 1);(0, 1);(1, 1);(1, 0);(1, -1);(0, -1);(-1, -1)|];;
		let array_of_matrix m =
			let (h,w) = ((Array.length m),(Array.length m.(0))) in
			let liste = Array.make (h*w-1) 0. in
			for i = 0 to (h*w-2) do
				let (k,l) = cells.(i) in
				liste.(i)<-m.(1-k).(1-l);
			done;liste;;

		(* Percentage of pi from tolerance *)
		let allowance tolerance = ((float_of_int tolerance)/.100.)*.pi;;

		(* Sum angles and get the sg type *)
		let sumAngles i j matrix tolerance =
			let ret = {x = i ; y = j ; typ = 3} in
			let liste = array_of_matrix matrix in
			let error = allowance tolerance in
			let sum = ref 0. in
			for cur = 0 to 7 do
				let next = ((cur+1) mod 8) in
				(if (abs_float (getAngleBetween liste.(cur) liste.(next))) > pi/.2. then
					liste.(next)<-liste.(next)+.pi;);
				sum := !sum +. getAngleBetween liste.(cur) liste.(next)
			done;
			if ((pi -. error) <= !sum) && (!sum <= (pi +. error)) then ret.typ<-(0);
			if (((-1.)*.pi -. error) <= !sum) && (!sum <= ((-1.)*.pi +. error)) then ret.typ<-(1);
			if ((2.*.pi -. error) <= !sum) && (!sum <= (2.*.pi +. error)) then ret.typ<-(2);
			ret;;

		(* Get all the singularity points *)
		let poincare_index matrix bloc_size tolerance =
			let (h,w) = ((Array.length matrix),(Array.length matrix.(0))) in
			let blocs = Images.makeBlocList (Orientation.getAngles matrix bloc_size) 3 in
			let ret = Array.make_matrix h w {x = 0 ; y = 0 ; typ = 3} in
			for i = 0 to ((Array.length blocs) - 1) do
				let (x,y) = (blocs.(i).x,blocs.(i).y) in
				ret.(x).(y)<-sumAngles x y blocs.(i).matrix tolerance
			done;
			ret;;

		(* Display singularity points *)
		let display_sp image bloc_size tolerance =
			let grey_im = Images.imageToGreyScale image in
			let sps = poincare_index grey_im.matrix bloc_size tolerance in
			open_graph (Images.getFormat image.width image.height);
			set_line_width 4;
			draw_image (make_image image.matrix) 0 0;
			for i = 1 to ((Array.length sps) - 1) do
				for j = 1 to ((Array.length sps.(0)) - 1) do
						if sps.(i).(j).typ < 3 then
						begin
							if sps.(i).(j).typ = 0 then set_color red; (* Loop *)
							if sps.(i).(j).typ = 1 then set_color green; (* Delta *)
							if sps.(i).(j).typ = 2 then set_color blue; (* Whorl *)
							let (x,y) = Orientation.getCircleLocation i j image.height bloc_size in
							moveto x y;
							draw_circle x y (bloc_size/2)
						end;
				done;
			done;;
	end
