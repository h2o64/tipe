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

	(* Segmentation part *)
	let segm_imag m bloc_size file_path =
			let (h,w) = Images.getHW m in
			let seg_level = Image_Processing.getOptimalThreshold_otsu m bloc_size in
			let seg = ref (Array.make_matrix h w 255.) in
			print_string "Segmentation ...\n";
			seg := (Image_Processing.segmentation m bloc_size seg_level);
			Testing.displayAnyMatrix !seg;
			dumpImage h w file_path "segmentation";
			(* Normalisation *)
			if (!seg = (Array.make_matrix h w 255.)) then
				(print_string "Normalisation ...\n";
				let norm = Image_Processing.normalisation m in
				dumpImage h w file_path "normalisation";
				(* New segmentation *)
				let seg_level_new = Image_Processing.getOptimalThreshold_otsu norm bloc_size in
				print_string "Segmentation Normalized ...\n";
				seg := (Image_Processing.segmentation norm bloc_size seg_level_new);
				Testing.displayAnyMatrix !seg;
				dumpImage h w file_path "segmentation");
			!seg;;


	(* Output orientation images *)
	let image_results () =
		(* Get images *)
		let file_list = listFP () in
		let image_list = makeImages file_list in
		let grey_images = greyImages image_list in
		let n = Array.length grey_images in
		let bloc_size = 16 in
		(* Do the commands *)
		Graphics.open_graph "";
		for i = 0 to (n-1) do
			(* Get sizes *)
			let (h,w) = (image_list.(i).height,image_list.(i).width) in
			Graphics.resize_window w h;
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
			let seg = ref (segm_imag grey_images.(i).matrix bloc_size file_list.(i)) in
			(* Get ROI *)
			print_string "ROI .. \n";
			let sobel_seg = Image_Processing.sobel_segmentation !seg true in (* Enable FFT *)
			Testing.align_matrix sobel_seg;
			print_string "ROI Extraction .. \n";
			let roi = Image_Processing.getROI sobel_seg in			
			(* Gabor *)
			print_string "Gabor .. \n";
			let gabor_tmp = Image_Processing.apply_gabor !seg bloc_size in
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
			Graphics.clear_graph ();
		done;;

	end
