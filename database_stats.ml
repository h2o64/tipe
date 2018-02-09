(* module type DB_STATS =
  sig

  end;;

module DB_Stats : DB_STATS = *)
module DB_Stats =
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
		sauver_image img_color bak_path;;

	(* Output orientation images *)
	let image_results () =
		(* Get images *)
		let file_list = listFP () in
		let image_list = makeImages file_list in
		let grey_images = greyImages image_list in
		let n = Array.length grey_images in
		(* Do the commands *)
		for i = 0 to (n-1) do
			(* Get sizes *)
			let (h,w) = (image_list.(i).height,image_list.(i).width) in
			(* Orientation *)
			Orientation.vector_field Orientation.getAngles image_list.(i) 16 true;
			dumpImage h w file_list.(i) "orientation";
			(* Poincare *)
			Poincare.display_sp image_list.(i) 16 48 Poincare.getAngleBetween;
			dumpImage h w file_list.(i) "poincare";
			(* Segmentation *)
			let seg_level = Image_Processing.getOptimalThreshold_otsu grey_images.(i).matrix 16 in
			let seg = Image_Processing.segmentation grey_images.(i).matrix 16 seg_level in
			Testing.displayAnyMatrix seg;
			dumpImage h w file_list.(i) "segmentation";
			(* Get ROI *)
			let sobel_seg = Image_Processing.sobel_segmentation seg  true in (* Force FFT *)
			Testing.align_matrix sobel_seg;
			let roi = Image_Processing.getROI sobel_seg in			
			(* Gabor *)
			let gabor = Image_Processing.getGabor grey_images.(i).matrix 16 in
			Testing.displayAnyMatrix gabor;
			dumpImage h w file_list.(i) "gabor";
			(* Binarisation *)
			let bin = Image_Processing.binarization gabor 16 in
			Testing.displayBin bin;
			dumpImage h w file_list.(i) "bin";
			(* Thinning *)
			let bin_roi = Image_Processing.keepROI_bin bin roi in
			let thin = Image_Processing.thinning bin_roi in
			Testing.displayBin thin;
			dumpImage h w file_list.(i) "thinned";
			(* Minutae *)
			Minutae.display_minutae thin;
			dumpImage h w file_list.(i) "minutae";
		done;;

	end
