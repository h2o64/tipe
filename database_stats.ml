(* Set correct Image processing module *)
module Image_Processing = Img_proc;;
open Image_Processing;;

(* Open Libraries *)
open Images;;
open Image_magick;;
open Minutae;;
open Orientation;;
open Poincare;;
open Testing;;

module DB_Stats :
  sig
    val listFP : unit -> string array
    val makeImages : string array -> Graphics.color Images.image array
    val greyImages :
      Graphics.color Images.image array -> float Images.image array
    val dumpImage : int -> int -> string -> string -> unit
    val image_results : unit -> unit
  end =
  struct

	(* Get actual files *)
	let listFP () =
		let ic = open_in "FP_DB.txt" in
		let files = really_input_string ic 2079 in
		let files_list = String.split_on_char '\n' files in
		Array.of_list files_list;;

	let makeImages file_list =
		let n = Array.length file_list in
		Array.init (n-1) (fun i -> Images.import_image file_list.(i));;

	let greyImages image_list =
		let n = Array.length image_list in
		Array.init n (fun i -> Images.imageToGreyScale image_list.(i));;

	(* Allow to dump image from graphics *)
	let dumpImage h w name suffix =
		let img = Graphics.get_image 0 0 w h in
		let img_color = Graphics.dump_image img in
		let s_l = String.length name in
		let bak_path = String.concat "" ["out/";
											Char.escaped name.[s_l-9];
											Char.escaped name.[s_l-8];
											Char.escaped name.[s_l-7];
											Char.escaped name.[s_l-6];
											Char.escaped name.[s_l-5];
											"-";
											suffix;".jpg"] in
		Image_magick.sauver_image img_color bak_path;;

	(* Output orientation images *)
	let image_results () =
		(* Get images *)
		let file_list = listFP () in
		let image_list = makeImages file_list in
		let grey_images = greyImages image_list in
		let n = Array.length grey_images in
		let bloc_size = 16 in
		(* Do the commands *)
		for i = 0 to (n-1) do
			(* Get sizes *)
			let (h,w) = (image_list.(i).height,image_list.(i).width) in
			(* DEBUG *)
			print_string "##### \nCurrent Image is : ";
			print_string file_list.(i);
			print_string "\n";
			(* Orientation *)
			print_string "Orientation .. \n";
			Orientation.vector_field Orientation.getAngles image_list.(i) bloc_size true;
			dumpImage h w file_list.(i) "orientation";
			(* Poincare *)
			print_string "Poincare .. \n";
			Poincare.display_sp image_list.(i) 16 48 Poincare.getAngleBetween;
			dumpImage h w file_list.(i) "poincare";
			(* Segmentation *)
			print_string "Segmentation level .. \n";
			let seg_level = Image_Processing.getOptimalThreshold_otsu grey_images.(i).matrix bloc_size in
			print_string "Segmentation .. \n";
			let seg = Image_Processing.segmentation grey_images.(i).matrix bloc_size seg_level in
			Testing.displayAnyMatrix seg;
			dumpImage h w file_list.(i) "segmentation";
			(* Get ROI *)
			print_string "ROI .. \n";
			let sobel_seg = Image_Processing.sobel_segmentation seg true in (* Enable FFT *)
			Testing.align_matrix sobel_seg;
			print_string "ROI Extraction .. \n";
			let roi = Image_Processing.getROI sobel_seg in			
			(* Gabor *)
			print_string "Gabor .. \n";
			let gabor_tmp = Image_Processing.apply_gabor seg bloc_size in
			Testing.align_matrix gabor_tmp;
			let gabor = Image_Processing.keepROI gabor_tmp roi in
			Testing.displayAnyMatrix gabor;
			dumpImage h w file_list.(i) "gabor";
			(* Binarisation *)
			print_string "Binarisation .. \n";
			let bin = Image_Processing.binarization gabor bloc_size in
			Testing.displayBin bin;
			dumpImage h w file_list.(i) "bin";
			(* Thinning *)
			print_string "Thinning .. \n";
			let bin_roi = Image_Processing.keepROI_bin bin roi in
			let thin = Image_Processing.thinning bin_roi in
			Testing.displayBin thin;
			dumpImage h w file_list.(i) "thinned";
			(* Minutae *)
			print_string "Minutae .. \n";
			Minutae.display_minutae thin;
			dumpImage h w file_list.(i) "minutae";
			print_string "DONE\n";
		done;;

	end
