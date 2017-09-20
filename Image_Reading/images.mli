type matrix = float array array
type bloc_size = int
type image = {
  height : int;
  width : int;
  matrix : Graphics.color array array;
}
type bw_image = { height : int; width : int; mutable matrix : matrix; }
type color = {
  mutable r : int;
  mutable g : int;
  mutable b : int;
  mutable a : int;
}
val getHeight : 'a array -> int
val getWidth : 'a array array -> int
val import_image : string -> image
val rgbint_to_color : int -> color
val grb_to_rgb : color -> unit
val color_to_rgbint : color -> int
val getFormat : int -> int -> string
val getLuminance : int -> float
val rgbToGreyScale : color -> float
val imageToGreyScale : image -> bw_image
val troncateImage : image -> bloc_size -> image

