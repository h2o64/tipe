(* Open Libraries *)
open Images;;
open Orientation;;
open Testing;;

module Minutae :
  sig
    type cn_pix = { typ : int; }
    type minutae = { x : int; y : int; theta : float; }
    val cells : (int * int) array
    val cn_local : int array array -> int -> int -> cn_pix
    val cn_global : int Images.matrix -> cn_pix array array
    val draw_minutae : int -> int -> int -> int -> Graphics.color -> unit
    val display_minutae : int Images.matrix -> unit
    val getMinutaeList : int Images.matrix -> minutae array * int
  end =
  struct

		(* 0 = not a minutia
			 1 = ridge ending
			 2 = intermediate ridge point
			 3 = bifurcation
			 4 = unknown *)
		type cn_pix = { typ : int };;
		(* x | y | orientation *)
		type minutae = {x : int ; y : int ; theta : float };;

		(* All paired pixels in 3x3 *)
		let cells = [|(-1, -1);(-1, 0);(-1, 1);(0, 1);(1, 1);(1, 0);(1, -1);(0, -1)|];;

		(* Get crossing number of pixel(i,j) *)
		let cn_local m i j =
			(* Get CN *)
			let crossings = ref 0 in
			for k = 1 to 8 do
				let (a,b) = cells.(k mod 8) in
				let (c,d) = cells.(k-1) in
				crossings := !crossings + abs (m.(i+a).(j+b) - m.(i+c).(j+d))
			done;
			{typ = !crossings / 2};;

		(* Execute crossing number on whole matrix *)
		let cn_global matrix =
			let (h,w) = Images.getHW matrix in
			let ret = Array.make_matrix h w {typ = 0} in
			for i = 1 to (h-2) do
				for j = 1 to (w-2) do
					if (matrix.(i).(j) = 1) then ret.(i).(j)<-(cn_local matrix i j);
				done;
			done;
			ret;;

		(* Draw circle *)
		let draw_minutae i j h l color =
			let (x,y) = Orientation.getCircleLocation i j h 1 in
			Graphics.set_color color;
			Graphics.moveto x y;
			Graphics.draw_circle x y l;;

		(* Display all minutae - Use binary matrix *)
		let display_minutae matrix =
			let (h,w) = Images.getHW matrix in
			Testing.displayBin matrix;
			(* Get CN matrix *)
			let cn_matrix = cn_global matrix in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					if (cn_matrix.(i).(j).typ = 1) then
						draw_minutae i j h 2 Graphics.red; (* Ridge Ending *)
					if false && (cn_matrix.(i).(j).typ = 2) then
						draw_minutae i j h 2 Graphics.green; (* Intermediate Ridge point *)
					if (cn_matrix.(i).(j).typ = 3) then
						draw_minutae i j h 2 Graphics.blue; (* Bifurcation *)
					if (cn_matrix.(i).(j).typ > 3) then
						draw_minutae i j h 2 Graphics.cyan; (* Unknown *)
				done;
			done;;

		(* Get minutae matrix *)
		let getMinutaeList matrix =
			let (h,w) = Images.getHW matrix in
			let cn_matrix = cn_global matrix in
			let orientation =
				(Orientation.getAngles (Images.applyFunctMatrix matrix float_of_int) 1) in
			let cur_minutae = ref 0 in
			let minutae_ret = Array.make (h*w) {x = -1; y = -1; theta = 10.} in (* 10. isn't an angle value *)
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					if (cn_matrix.(i).(j).typ = 1) || (cn_matrix.(i).(j).typ = 3) then
						(minutae_ret.(!cur_minutae) <- { x = i ; y = j ; theta = orientation.(i).(j)};
						cur_minutae := !cur_minutae + 1);
				done;
			done;(minutae_ret,!cur_minutae);;

	end
