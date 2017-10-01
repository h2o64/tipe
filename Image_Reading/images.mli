type 'a matrix = 'a array array
type 'a bloc = { x : int; y : int; matrix : 'a matrix; }
type 'a pixel_blocs = 'a bloc list
type 'a image = { height : int; width : int; mutable matrix : 'a matrix; }
val getHeight : 'a matrix -> int
val getWidth : 'a matrix -> int
val matrixApply : ('a -> 'b) -> 'a matrix -> 'b matrix
val import_image : string -> Graphics.color image
val getFormat : int -> int -> string
val color_of_rgbint : Graphics.color -> int * int * int
val greyscale_of_rgb : Graphics.color -> float
val rgb_of_greyscale : float -> int
val imageToGreyScale : Graphics.color image -> float image
val bwimageToImage : float image -> int image
val getSurrounding : int -> int -> 'a matrix -> int -> int -> int -> 'a matrix
val makeBlocList : 'a matrix -> int -> 'a bloc array
val troncateImage : 'a image -> int -> 'a image
val transpose : 'a matrix -> 'a matrix
