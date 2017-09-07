(* Open Image library *)
open Graphics ;;
open Images ;;

(* API *)
type image = {height : int ; width : int ; matrix : Graphics.color array array}
type color = {mutable r : int ; mutable g : int ; mutable b : int ; mutable a : int}

(* Image properties *)
(*
	getHeight : 'a array -> int
	getWidth : 'a array array -> int
 *)
let getHeight img = Array.length img;;
let getWidth img = Array.length img.(0);;

(* Import image in struct *)
(* import_image : string -> image *)
let import_image file = let matrix = lire_image file in
			{height = getHeight matrix; width = getWidth matrix; matrix = matrix};;

(* RGBInt <-> RGB Color *)
(*
	rgbint_to_color : int -> color
	grb_to_rgb : color -> unit
	color_to_rgbint : color -> int
	NOTE: ImageMagick outputs (B,G,R,A) not (R,G,B,A) tho, we'll use this convention
 *)
let rgbint_to_color num =
	(* Red/Green/Blue/transpArancy *)
	let b = num mod 256 in
	let g = (num/256) mod 256 in
	let r = (num/256/256) mod 256 in
	let a = (num/256/256/256) in
	{r = r; g = g; b = b; a = a};;
let grb_to_rgb cl = let x = cl.r in
			cl.r <- cl.b;
			cl.b <- x;;
let color_to_rgbint cl = cl.r + cl.g*256 + cl.b*256*256 + cl.a*256*256*256;;

(* Open my test image
let test_image = import_image "pepe-meme.jpg" *)
