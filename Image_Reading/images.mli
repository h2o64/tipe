(**
   Module Images
   Permet de charger et sauvegarder des images stockées dans des fichiers dans divers formats : png, jpg, gif, bmp, pgm, ppm.
   Necessite la suite logicielle ImageMagick.
   L'utilisation de ce module en mode interprete necessite l'appel à l'interpreteur avec les options :
   - [ocaml graphics.cma images.cmo]
   La production d'un executable utilisant ce module doit se faire avec la commande :
   - [ocamlc -o <nom_executable>  graphics.cma images.cmo  <source_a_compiler>]
   @author FIL - IEEA - Univ. Lille1 (mars 2010) / Louis Popi - MP (September 2017)
   @see <http://www.imagemagick.org/> le site d'ImageMagick.
*)

type image = {
  height : int;
  width : int;
  matrix : Graphics.color array array;
}
type color = {
  mutable r : int;
  mutable g : int;
  mutable b : int;
  mutable a : int;
}
val liste_formats : string list
val lire_image : string -> Graphics.color array array
val sauver_image : Graphics.color array array -> string -> unit
val dessiner_image : Graphics.color array array -> unit
val getHeight : 'a array -> int
val getWidth : 'a array array -> int
val import_image : string -> image
val rgbint_to_color : int -> color
val grb_to_rgb : color -> unit
val color_to_rgbint : color -> int
val getFormat : int -> int -> string

