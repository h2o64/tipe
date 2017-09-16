(*
  auteur : FIL / Louis Popi
  date   : septembre 2017
  objet  : lecture et sauvegarde de fichiers image dans différents format 
   (PGM, PPM, JPG, PNG, GIF, ...)
  necessite l'installation de la commande convert de la suite de traitements
  d'images Image Magick (http://www.imagemagick.org/)
*)

type image = {height : int ; width : int ; matrix : Graphics.color array array}
type color = {mutable r : int ; mutable g : int ; mutable b : int ; mutable a : int}

let suffixe_tmp = ".tmp "
and rm = 
  if Sys.os_type="Unix" then
    "rm -f "
  else
    "del " 
and mv = 
  if Sys.os_type="Unix" then
    "mv "
  else
    "move " 
and dev_null =
  if Sys.os_type="Unix" then
    " 2> /dev/null"
  else
    "" 
open Graphics 

let lire_image_ppm nom = 
  let entree = open_in_bin nom 
  in
    let format = input_line entree 
    and largeur,hauteur = 
      let ligne = ref (input_line entree)
      in
	while !ligne.[0] = '#' do
	  ligne := input_line entree
	done ;
	Scanf.sscanf 
	  !ligne 
          "%d %d" 
	  (fun x y -> x,y)
    and _ = input_line entree (* lecture de la ligne contenant 255 *)
    in
      let img = Array.make_matrix hauteur largeur (rgb 0 0 0)
      and en_couleur = (format = "P6")
      in
	for i = 0 to hauteur - 1 do
	  for j = 0 to largeur - 1 do
	    img.(i).(j) <- 
	      if en_couleur then
		let x = input_byte entree
		and y = input_byte entree
		and z = input_byte entree
		in 
		  rgb x y z
	      else
		let x = input_byte entree 
		in 
		  rgb x x x
	  done
	done ;
	close_in entree ;
	img 

let lire_image nom =
  let r = Sys.command ("convert -depth 8 "^nom^" "^nom^".ppm "^dev_null)
  in
    if r <> 0 then
      failwith ("lire_image : fichier "^nom^" manquant ou pas dans un format image")
    else
      let res = lire_image_ppm (nom^".ppm")
      in
	ignore(Sys.command (rm^nom^".ppm"));
	res

let dessiner_image img =
  draw_image (make_image img) 0 0;
  let _ = read_key() in close_graph();;

let sauver_image_ppm (img : Graphics.color array array) nom = 
  let sortie = open_out_bin nom
  and hauteur = Array.length img
  and largeur = Array.length img.(0)
  in
    output_string sortie "P6\n" ;
    output_string sortie ((string_of_int largeur)^" "^(string_of_int hauteur)^"\n") ;
    output_string sortie "255\n";
    for i = 0 to hauteur - 1 do
      for j = 0 to largeur - 1 do
	let r = img.(i).(j) / (256*256)
	and g = (img.(i).(j) mod (256*256)) / 256
	and b = img.(i).(j) mod 256
	in
	  output_byte sortie r ;
	  output_byte sortie g ;
	  output_byte sortie b
      done
    done ;
    close_out sortie


let liste_formats = [".png"; ".jpg"; ".gif"; ".bmp"; ".pgm"; ".ppm"] 

let sauver_image img nom = 
  let suffixe = String.sub nom ((String.length nom) - 4) 4
  in
    if not (List.mem suffixe liste_formats) then
      failwith "sauver_image : format image non reconnu"
    else
      let _ = sauver_image_ppm img (nom^".tmp")
      in
	if suffixe="ppm" then
	  ignore(Sys.command (mv^nom^suffixe_tmp^nom))
	else begin
	  ignore(Sys.command ("convert "^nom^suffixe_tmp^" "^nom^dev_null)) ;
	  ignore(Sys.command (rm^nom^suffixe_tmp))
	end

let getHeight img = Array.length img;;
let getWidth img = Array.length img.(0);;

let import_image file = let matrix = lire_image file in
			{height = getHeight matrix; width = getWidth matrix; matrix = matrix};;

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

(* Get the right image format *)
let getFormat height width =
	let s_height = string_of_int height in
	let s_width = string_of_int width in
	String.concat "" [" ";s_height;"x";s_width];;
