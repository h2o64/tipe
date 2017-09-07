(* Open Image library *)
open Graphics ;;
open Images ;;

let image = lire_image "pepe-meme.jpg";;

let hauteur img = Array.length img;;
let largeur img = Array.length img.(0);;
let rgbint_to_color num =
	(* Red/Green/Blue/transpArancy *)
	let b = num mod 256 in
	let g = (num / 256) mod 256 in
	let r = (num / 256 / 256) mod 256 in
	let a = (num / 256 / 256 / 256) in
	(r,g,b,a);;



