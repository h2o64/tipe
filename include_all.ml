(* Graphic Library *)
#load "graphics.cma";;

(* Unix module *)
#load "unix.cma";;

(* FFTW3 Module *)
#load "fftw3.cma";;

(* Open libraries *)
open Graphics;;
open Fftw3;;

(* Set FFT Module *)
module FFT = Fftw3.D;;					

(* Open modules *)
#use "image_magick.ml";;
#use "images.ml";;
#use "strassen.ml"
#use "convolution.ml";;
#use "testing.ml";;
#use "orientation.ml";;
#use "poincare.ml";;
#use "plot.ml";;
#use "frequency.ml";;
#use "img_proc.ml";;
#use "minutae.ml";;
#use "hough_transform.ml";;
#use "database_stats.ml";;
