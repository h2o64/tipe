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
open Hough_transform;;

module DB_Stats :
	sig
    val listFP : unit -> string array
    val makeImages : string array -> Graphics.color Images.image array
    val greyImages :
      Graphics.color Images.image array -> float Images.image array
    val dumpImage : int -> int -> string -> string -> string -> unit
    val segm_imag :
      float Images.matrix ->
      int -> string -> string -> bool -> float array array
    val image_results : unit -> unit
  end =
  struct

	(* Get actual files *)
	let listFP () =
		(* let ic = open_in "FP_DB.txt" in
		let files = really_input_string ic 8319 in *)
		let ic = open_in "FP_DB3.txt" in
		let files = really_input_string ic 2080 in
		let files_list = String.split_on_char '\n' files in
		Array.of_list files_list;;

	let makeImages file_list =
		let n = Array.length file_list in
		Array.init (n-1) (fun i -> Images.import_image file_list.(i));;

	let greyImages image_list =
		let n = Array.length image_list in
		Array.init n (fun i -> Images.imageToGreyScale image_list.(i));;

	(* Allow to dump image from graphics *)
	let dumpImage h w name suffix plan_name =
		if false then
			(let img = Graphics.get_image 0 0 w h in
			let img_color = Graphics.dump_image img in
			let s_l = String.length name in
			let bak_path = String.concat "" ["out3/";
												plan_name;
												Char.escaped name.[s_l-15];
												Char.escaped name.[s_l-14];
												Char.escaped name.[s_l-13];
												Char.escaped name.[s_l-12];
												Char.escaped name.[s_l-11];
												Char.escaped name.[s_l-10];
												Char.escaped name.[s_l-9];
												Char.escaped name.[s_l-8];
												Char.escaped name.[s_l-7];
												Char.escaped name.[s_l-6];
												Char.escaped name.[s_l-5];
												"-";
												suffix;".jpg"] in
			Image_magick.sauver_image img_color bak_path)
		else ();;

	(* Segmentation part *)
	let segm_imag m bloc_size file_path plan_name doNormalisation =
			let (h,w) = Images.getHW m in
			let seg_level = Image_Processing.getOptimalThreshold_otsu m bloc_size in
			let seg = ref (Array.make_matrix h w 255.) in
			print_string "Segmentation ...\n";
			seg := (Image_Processing.segmentation m bloc_size seg_level);
			Testing.displayAnyMatrix !seg;
			dumpImage h w file_path "segmentation" plan_name;
			(* Normalisation *)
			if (!seg = (Array.make_matrix h w 255.)) && doNormalisation then
				(print_string "Normalisation ...\n";
				let norm = Image_Processing.normalisation m in
				dumpImage h w file_path "normalisation" plan_name;
				(* New segmentation *)
				let seg_level_new = Image_Processing.getOptimalThreshold_otsu norm bloc_size in
				print_string "Segmentation Normalized ...\n";
				seg := (Image_Processing.segmentation norm bloc_size seg_level_new);
				Testing.displayAnyMatrix !seg;
				dumpImage h w file_path "segmentation") plan_name;
			!seg;;


	(* Dump timings CSV *)
	let dumpTime filename timings plan_name =
		let s_l = String.length filename in
		let num = String.concat "" [
											Char.escaped filename.[s_l-9];
											Char.escaped filename.[s_l-8];
											Char.escaped filename.[s_l-7];
											Char.escaped filename.[s_l-6];
											Char.escaped filename.[s_l-5]] in
		let db = String.concat "" [
											Char.escaped filename.[s_l-15];
											Char.escaped filename.[s_l-14];
											Char.escaped filename.[s_l-13];
											Char.escaped filename.[s_l-12];
											Char.escaped filename.[s_l-11]] in
		let ret = String.concat ";" [
											db;
											num;
											(string_of_float timings.(0));
											(string_of_float timings.(1));
											(string_of_float timings.(2));
											(string_of_float timings.(3));
											(string_of_float timings.(4));
											(string_of_float timings.(5));
											(string_of_float timings.(6));
											(string_of_float timings.(7));
											(string_of_float timings.(8));
											(string_of_float timings.(9));
											(string_of_float timings.(10));
											"\n"] in
		(* Write the file *)
		let file_name = (String.concat plan_name ["out3/";"benchmark-time.csv"]) in
		let file = (open_out_gen [Open_append] 666 file_name) in
		output_string file ret;
		close_out file;;

	(* Dump hough transform results *)
	let convertToCSV file_list hough_results plan_name =
		let n = (Array.length hough_results) in
		let file_name = (String.concat plan_name ["out3/";"benchmark-hough.csv"]) in
		let file = (open_out_gen [Open_append] 666 file_name) in
		(* Make header *)
		print_string "convertToCSV: Making header\n";
		let header = ref [] in
		for i = 0 to (n-1) do
			let s_l = String.length file_list.(i) in
			let num = String.concat "" [
												Char.escaped file_list.(n-i-1).[s_l-9];
												Char.escaped file_list.(n-i-1).[s_l-8];
												Char.escaped file_list.(n-i-1).[s_l-7];
												Char.escaped file_list.(n-i-1).[s_l-6];
												Char.escaped file_list.(n-i-1).[s_l-5]] in
			header := num::!header;
		done;
		(* Write header *)
		print_string "convertToCSV: Writing header\n";
		output_string file (String.concat ";" (""::!header));
		output_string file "\n";
		for i = 0 to (n-1) do
			let s_l = String.length file_list.(n-i-1) in
			let num = String.concat "" [
												Char.escaped file_list.(n-i-1).[s_l-9];
												Char.escaped file_list.(n-i-1).[s_l-8];
												Char.escaped file_list.(n-i-1).[s_l-7];
												Char.escaped file_list.(n-i-1).[s_l-6];
												Char.escaped file_list.(n-i-1).[s_l-5]] in
			let ret = ref [] in
			for j = 0 to (n-1) do
				ret := (string_of_float hough_results.(i).(j))::!ret;
			done;
			(* Write the file *)
			print_string "convertToCSV: Writting line\n";
			output_string file (String.concat ";" (num::!ret));
			output_string file "\n";
		done;
		close_out file;;

	(* Output orientation images *)
	let image_results () =
		(* Get images *)
		let file_list = listFP () in
		let image_list = makeImages file_list in
		let grey_images = greyImages image_list in
		let n = Array.length grey_images in
		let bloc_size = 16 in
		(* Plans *)
		(* Scheme : [doNormalisation;doSegmentation;segOnly;dumpMinutae] *)
		(* Plan 1 [SegOnly] : Normalisation + Segmentation
			 Plan 2 [SegOnly] : No normalisation + Segmentation
			 Plan 1B : Normalisation + Segmentation
			 Plan 2B: No normalisation + Segmentation
			 Plan 3: Normalisation + No segmentation
			 Plan 4: No normalisation + No segmentation *)
		let plan1 = [|true;true;true;false|] in
		let plan2 = [|false;true;true;false|] in
		let plan1b = [|true;true;false;true|] in
		let plan2b = [|false;true;false;true|] in
		let plan3 = [|true;false;false;true|] in
		let plan4 = [|false;false;false;true|] in
		let plans = [|(plan1,"plan1/");
									(plan2,"plan2/");
									(plan1b,"plan1b/");
									(plan2b,"plan2b/");
									(plan3,"plan3/");
									(plan4,"plan4/")|] in
		(* Do the commands *)
		Graphics.open_graph "";
		for plan_num = 0 to 5 do
			let (plan,plan_name) = plans.(plan_num) in
			let doNormalisation = plan.(0) in
			let doSegmentation = plan.(1) in
			let segOnly = plan.(2) in
			let dumpMinutae = plan.(3) in
			(* Make massive minutae array *)
			let minu_values = ref [] in
			for i = 0 to (n-1) do
				(* Get sizes *)
				let (h,w) = (image_list.(i).height,image_list.(i).width) in
				Graphics.resize_window w h;
				(* DEBUG *)
				print_string "##### \nCurrent Image is : ";
				print_string file_list.(i);
				print_string "\n";
				(* Time benchmark *)
				(* Timing structure :
				[0] : Starting time
				[1] : Orientation (else [0])
				[2] : Poincare Index (else [0])
				[3] : Segmentation (else [prev])
				[4] : Normalisation (else [prev])
				[5] : ROI
				[6] : Gabor
				[7] : Binarisation
				[8] : Thining Guo Hall
				[9] : Thining Zhangsuen
				[10] : Crossing Numbers *)
				let start_time = (Unix.gettimeofday ()) in
				let timings = Array.make 11 start_time in
				if not segOnly then
					(* Orientation *)
					(print_string "Orientation .. \n";
					Orientation.vector_field Orientation.getAngles image_list.(i) bloc_size true;
					timings.(1) <- (Unix.gettimeofday () -. start_time);
					dumpImage h w file_list.(i) "orientation" plan_name;
					(* Poincare *)
					print_string "Poincare .. \n";
					Poincare.display_sp image_list.(i) 16 48 Poincare.getAngleBetween;
					dumpImage h w file_list.(i) "poincare" plan_name;
					timings.(2) <- (Unix.gettimeofday () -. start_time))
				else
					(* Reset initial timings *)
					(timings.(1) <- (start_time);
					timings.(2) <- (start_time));
				(* Segmentation *)
				let seg =
					if doSegmentation then
						ref (segm_imag grey_images.(i).matrix bloc_size file_list.(i) plan_name doNormalisation)
					else
						ref (grey_images.(i).matrix) in
				timings.(3) <- (Unix.gettimeofday () -. start_time);
				if not segOnly then
					(* Normalisation *)
					(if doNormalisation then
						(print_string "Normalisation ...\n";
						seg := Image_Processing.normalisation !seg;
						dumpImage h w file_list.(i) "normalisation" plan_name;);
					timings.(4) <- (Unix.gettimeofday () -. start_time);
					(* Get ROI *)
					print_string "ROI .. \n";
					let sobel_seg = Image_Processing.sobel_segmentation !seg true in (* Enable FFT *)
					Testing.align_matrix sobel_seg;
					print_string "ROI Extraction .. \n";
					let roi = Image_Processing.getROI sobel_seg in
					timings.(5) <- (Unix.gettimeofday () -. start_time);	
					(* Gabor *)
					print_string "Gabor .. \n";
					let gabor_tmp = Image_Processing.apply_gabor !seg bloc_size in
					Testing.align_matrix gabor_tmp;
					let gabor = Image_Processing.keepROI gabor_tmp roi in
					timings.(6) <- (Unix.gettimeofday () -. start_time);	
					Testing.displayAnyMatrix gabor;
					dumpImage h w file_list.(i) "gabor" plan_name;
					(* Binarisation *)
					print_string "Binarisation .. \n";
					let bin = Image_Processing.binarization gabor bloc_size in
					timings.(7) <- (Unix.gettimeofday () -. start_time);	
					Testing.displayBin bin;
					dumpImage h w file_list.(i) "bin" plan_name;
					(* Thinning *)
					print_string "Thinning .. \n";
					let bin_roi = Image_Processing.keepROI_bin bin roi in
					let thin_g = Image_Processing.thinning bin_roi Image_Processing.one_thining_guohall in
					timings.(8) <- (Unix.gettimeofday () -. start_time);	
					Testing.displayBin thin_g;
					dumpImage h w file_list.(i) "thinned-guohall" plan_name;
					let thin = Image_Processing.thinning bin_roi Image_Processing.one_thining_zhangsuen in
					timings.(9) <- (Unix.gettimeofday () -. start_time);	
					Testing.displayBin thin;
					dumpImage h w file_list.(i) "thinned-zhangsuen" plan_name;
					(* Minutae *)
					print_string "Minutae .. \n";
					Minutae.display_minutae thin_g;
					timings.(10) <- (Unix.gettimeofday () -. start_time);	
					dumpImage h w file_list.(i) "minutae" plan_name;
					(* Dump time *)
					dumpTime file_list.(i) timings plan_name;
					(* Dump minutae *)
					if dumpMinutae then
						(minu_values := (Minutae.getMinutaeList thin_g)::!minu_values)
					);
				print_string "DONE\n";
				Graphics.clear_graph ();
			done;
			(* Do mass comparaison *)
			if dumpMinutae then
				(let rev_list l =
				let rec rev_acc acc = function
					| [] -> acc
					| hd::tl -> rev_acc (hd::acc) tl
				in rev_acc [] l in
				let reversed_minu = rev_list !minu_values in
				let minu_arr = Array.of_list reversed_minu in
				(* Goal :
					- Let A = set of first fingerprint of NUM
					- Let B = set of second fingerprint of NUM
					- Let C = set of first fingerprint of NUM+1
					- Let D = set of second fingerprint of NUM+1
					-> Compare A with B
					-> Compare A with C
					-> Compare C with D
					-> Compare B with D *)
				(* Create sets *)
				let a = ref minu_arr.(0) in
				let b = ref minu_arr.(1) in
				let c = ref minu_arr.(8) in
				let d = ref minu_arr.(9) in
				let last_ind = ref (0,1,8,9) in
				let ret_pos = ref [] in
				let ret_rand = ref [] in
				while (let (_,_,_,tmp) = !last_ind in tmp+4 < n) do
					(* Get scores *)
					ret_pos := (string_of_float (Hough_transform.getScore !a !b))::!ret_pos;
					ret_pos := (string_of_float (Hough_transform.getScore !c !d))::!ret_pos;
					ret_rand := (string_of_float (Hough_transform.getScore !a !c))::!ret_rand;
					ret_rand := (string_of_float (Hough_transform.getScore !b !d))::!ret_rand;
					(* Reset for new sets *)
					let (cur_a,cur_b,cur_c,cur_d) = !last_ind in
					a := minu_arr.(cur_a+4);
					b := minu_arr.(cur_b+4);
					c := minu_arr.(cur_c+4);
					d := minu_arr.(cur_d+4);
					last_ind := (cur_a+4,cur_b+4,cur_c+4,cur_d+4);
				done;
				(* Write the file *)
				let file_name_pos = (String.concat plan_name ["out3/";"benchmark-hough-pos.txt"]) in
				let file_name_rand = (String.concat plan_name ["out3/";"benchmark-hough-rand.txt"]) in
				let file_pos = (open_out_gen [Open_append] 666 file_name_pos) in
				let file_rand = (open_out_gen [Open_append] 666 file_name_rand) in
				output_string file_pos (String.concat "\n" !ret_pos);
				output_string file_rand (String.concat "\n" !ret_rand);
				close_out file_pos;
				close_out file_rand;
				);					
		done;;
	end



















