(* Libraries *)
open Image_magick;;

(* Structures *)
type 'a matrix = 'a array array;;
type bloc_size = int;;
type image = {height : int ; width : int ; mutable matrix : Graphics.color matrix};;
type bw_image = {height : int ; width : int ; mutable matrix : float matrix};;

(* Get height and width of an image *)
let getHeight (img : 'a matrix) = Array.length img;;
let getWidth (img : 'a matrix) = Array.length img.(0);;

(* Import an image *)
let import_image file = let matrix = lire_image file in
		({height = getHeight matrix; width = getWidth matrix; matrix = matrix} : image);;

(* Get the right image format *)
let getFormat height width =
	let s_height = string_of_int height in
	let s_width = string_of_int width in
	String.concat "" [" ";s_height;"x";s_width];;

(* Convert RGB integer to color type *)
let rgbint_to_color (num : Graphics.color) =
	(* Red/Green/Blue *)
	let b = num mod 256 in
	let g = (num/256) mod 256 in
	let r = (num/256/256) mod 256 in
	(r,g,b);;

(* Get pixel luminance *)
let getLuminance pix =
(* Relative luminance in colorimetric spaces
	 Luminance (Standard for certain colour spaces): (0.2126*R + 0.7152*G + 0.0722*B)
	 Luminance (Option 1): (0.299*R + 0.587*G + 0.114*B)
	 Luminance (Option 2): sqrt( 0.299*R^2 + 0.587*G^2 + 0.114*B^2 )
	 Source:
		* [1] Wikipédia - Relative luminance
		* [2] W3.org - https://www.w3.org/TR/AERT#color-contrast
		* [3] Darel Rex Finley - http://alienryderflex.com/hsp.html *)
	 let (r,g,b) = rgbint_to_color pix in
	 let luminance = 0.299 *. (float_of_int (r) ** 2.)
								+. 0.587 *. (float_of_int (g) ** 2.)
								+. 0.114 *. (float_of_int (b) ** 2.) in sqrt luminance;;

(* Convert RGB to greyscale *)
let rgbToGreyScale pix = let (r,g,b) = rgbint_to_color pix in
			(float_of_int (r + g + b))/.3.;;

(* Convert whole image to greyscale *)
let imageToGreyScale (image : image) =
		let (h,w) = (image.height,image.width) in
		let ret = Array.make_matrix h w 0. in
		for i = 0 to (h-1) do
			for j = 0 to (w-1) do
				ret.(i).(j) <- rgbToGreyScale image.matrix.(i).(j)
			done;
		done;
		({height = h; width = w; matrix = ret} : bw_image);;

(* Troncate image for great bloc size *)
let troncateImage (image : image) (bloc_size : bloc_size) =
	let h = image.height - (image.height mod bloc_size) in
	let w = image.width - (image.width mod bloc_size) in
	let ret = (Array.make_matrix h w 0) in
	for i = 0 to (h-1) do
		for j = 0 to (w-1) do
			ret.(i).(j) <- image.matrix.(i).(j)
		done;
	done;
	({ height = h ; width = w ; matrix = ret } : image);;
