(* Open Libraries *)
open Frequency;;

module Plot :
  sig
    val max_array : int array -> int * int
    val plot :
      int array * int array * int * int * bool * Graphics.color * string *
      int * bool * int * bool * bool -> unit
    val int_of_float_array : float array -> int array
    val integersArray : int -> int -> int array
    val floatsArray : int -> int -> float array
    val plot_array :
      int array * int * int * bool * Graphics.color * string * int * 
      bool * int * bool * bool -> unit
    val plot_mouse : int -> Graphics.color -> int -> int * int
  end  =
  struct

		(* Maximum of an int array *)
		let max_array tab =
			let maxi = ref (-1,-1) in
			for i = 0 to ((Array.length tab)-1) do
				let (a,b) = !maxi in
				if tab.(i) > b then maxi := (i,tab.(i))
			done;!maxi;;

		(* Plot a discrete amount of variables *)
		let plot (x,y,x_offset,x_factor,bars,color,title,line_width,doResize,start_at,doScaleHeight,showHighest) =
			(* Width/Height of the values *)
			let (max_ind,max_val) = max_array y in
			let (width,height) = (Array.length x,max_val) in
			let x_old = ref (-1) in
			let y_old = ref (-1) in
			let y_factor = ref 1 in
			(* Create the graphic window *)
			y_old := Graphics.size_y ();
			if doResize then
				(x_old := Graphics.size_x ();
				if doScaleHeight then Graphics.resize_window (!x_old + width*x_factor) (!y_old)
				else Graphics.resize_window (!x_old + width*x_factor) (height);)
			else (x_old := x_offset);
			if (height > !y_old) && doScaleHeight then y_factor := height/(!y_old);
			(* Check if the sizes are right *)
			if not (Array.length x = Array.length y) then failwith "Error in array lengths";
			(* Make the points *)
			let points = Array.make width (0,0,0,0) in
			let prev = ref (x.(0),y.(0)) in
			for i = start_at to width-1 do
				if bars then
					let (a,b) = (!x_old+x.(i)*x_factor,y.(i)/(!y_factor)) in
					points.(i)<-(a,0,a,b);
				else
					let (a,b) = !prev in
					points.(i) <- (!x_old+a*x_factor,b/(!y_factor),!x_old+x.(i)*x_factor,y.(i)/(!y_factor));
					prev := (x.(i),y.(i));
			done;				
			Graphics.set_window_title title;
			Graphics.set_line_width line_width;
			Graphics.set_color color;
			Graphics.draw_segments points;
			(if not (color = Graphics.blue) then Graphics.set_color Graphics.blue
			else Graphics.set_color Graphics.green);
			if showHighest then
				let text_bar = 20 in
				let (cur_x,cur_y) = (Graphics.size_x (),Graphics.size_y ()) in
				let x_text = (((cur_x - !x_old)/2)-50) + !x_old in
				let y_text = cur_y - (text_bar/2) - 10 in
				if doResize then Graphics.resize_window cur_x (cur_y + text_bar);
				Graphics.draw_segments [|x.(max_ind)*x_factor+(!x_old),0,!x_old+x.(max_ind)*x_factor,max_val|];
				Graphics.moveto x_text y_text;
				Graphics.draw_string (String.concat "" ["Ridge Frequency = ";string_of_int (x.(max_ind)+1)]);;

		(* float array to int array *)
		let int_of_float_array tab = Array.map int_of_float tab;;

		(* Integers for start to stop *)
		let integersArray start stop =
			let ret = Array.make ((stop-start)+1) 0 in
			let i = ref 0 in
			for j = start to stop do
				ret.(!i) <- j;
				i := !i + 1;
			done;ret;;

		(* Floats for start to stop *)
		let floatsArray start stop =
			let ret = Array.make ((stop-start)+1) 0. in
			let i = ref 0 in
			for j = start to stop do
				ret.(!i) <- (float_of_int j);
				i := !i + 1;
			done;ret;;

		(* Plot a function *)
		(* FIXME: Broken *)
		(* let plof_fct (f,start,stop,x_factor,bars,color,title,line_width,doResize,start_at,doScaleHeight) =
			let x = floatsArray start stop in
			let y = Array.map f x in
			let x_new = int_of_float_array x in
			let y_new = int_of_float_array y in
			plot (x_new,y_new,x_offset,x_factor,bars,color,title,line_width,doResize,start_at,doScaleHeight);; *)

		(* Plot an array *)
		let plot_array (tab,x_offset,x_factor,bars,color,title,line_width,doResize,start_at,doScaleHeight,showHighest) =
			let x = integersArray 0 ((Array.length tab)-1) in
			plot (x,tab,x_offset,x_factor,bars,color,title,line_width,doResize,start_at,doScaleHeight,showHighest);;

		(* Mouse tracking *)
		let plot_mouse size color width =
				let (i,j) = (Graphics.mouse_pos ()) in
				Graphics.set_color color;
				Graphics.set_line_width width;
				Graphics.draw_circle i j (size/2);
				(i,j);;

	end
