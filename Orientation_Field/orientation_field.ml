(* Open image to analyse *)
(* let test_image = import_image "../Orientation_Field/fingerprint.jpg" *)

(* Convert image to black and white only *)
let convert_black img =
	let bak = img in
	for action = 0 to 1 do
		for i = 1 to (img.height-2) do
			for j = 1 to (img.width-2) do
				if action = 0 then (* Remove less luminant parts *)
					if getLuminance img.matrix.(i).(j) > 70.0 then bak.matrix.(i).(j)<- 16777215
				else (* Fill holes *)
					let detector = ref true in
					for k = (i-1) to (i+1) do
						(* Check if the 3x3 chunk is dark *)
						for l = (j-1) to (j+1) do
							if (k != i) && (l != j) then
								detector := !detector && getLuminance bak.matrix.(k).(l) <= 35.0
						done;
					done;
					if !detector then bak.matrix.(i).(j)<- 0
			done;
		done;
	done;
	bak;;

(* Rotate picture *)
let transpose (m: int array array) =
	let n = Array.make_matrix (Array.length m.(0)) (Array.length m) 0 in
	for i = 0 to (Array.length m - 1) do
		let row_i = m.(i) in
			for j = 0 to (Array.length row_i - 1) do
				n.(j).(i) <- row_i.(j)
			done;
		done;
	n;;

(* Smooth-ed image *)
let new_test_img = convert_black test_image;;

(* Change an array with white and black dots *)
let makeDot x y tab =
	for i = x to y do
		tab.(i) <- 16777215
	done;;

(* Make a map with all the averega ridge location *)
let make_point_map img =
	let t_img = transpose img.matrix in
	let bak = t_img in
	for i = 1 to (img.width-1) do
		let first = ref (-1) in
		let last = ref 1 in
		for j = 1 to (img.height-1) do
			if (!first < 0) && (getLuminance t_img.(i).(j) < 50.0) then
				first := j;
			makeDot !last j bak.(i); (* Set all to white *)
			if (!first >= 0) && (getLuminance t_img.(i).(j) >= 50.0) then
				(bak.(i).((!first+j)/2)<- 0;
				first := -1;
				last := j+1)
		done;
	done;
	open_graph (getFormat img.width img.height);
	dessiner_image (transpose bak);;
