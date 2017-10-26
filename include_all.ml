(* Graphic Library *)
#load "graphics.cma";;

(* Unix module *)
#load "unix.cma";;

(* ImagesMagick Module *)
#load "image_magick.cmo";;

(* Open libraries *)
open Graphics;;
open Unix;;
open Image_magick;;

(* Open modules *)
#use "images.ml";;
#use "convolution.ml";;
#use "orientation.ml";;
#use "poincare.ml";;
#use "testing.ml";;
