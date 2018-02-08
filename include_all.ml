(* Graphic Library *)
#load "graphics.cma";;

(* Unix module *)
#load "unix.cma";;

(* ImagesMagick Module *)
#load "image_magick.cmo";;

(* FFTW3 Module *)
#load "fftw3.cma";;

(* Open libraries *)
open Graphics;;
open Unix;;
open Image_magick;;
open Format;;
open Fftw3;;

(* Set FFT Module *)
module FFT = Fftw3.D;;					

(* Open modules *)
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
