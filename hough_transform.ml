(* Open Libraries *)
open Minutae;;
open Poincare;;

module Hough_transform :
  sig
    val delta :
      Minutae.minutae array -> Minutae.minutae array -> int -> int -> float
    val getParameters :
      Minutae.minutae array * int ->
      Minutae.minutae array * int -> float array * float array * float array
    val voteParameters :
      Minutae.minutae array * int ->
      Minutae.minutae array * int -> float * float * float
    val getScore :
      Minutae.minutae array * int -> Minutae.minutae array * int -> float
  end =
  struct

		(* Define delta(i,j) *)
		let delta (mI : Minutae.minutae array) (mT : Minutae.minutae array) i j =
			let delta0 i j = abs_float (mI.(i).theta -. mT.(j).theta) in
			let delta_tmp = delta0 i j in
			if delta_tmp <= Poincare.pi then delta_tmp
			else ((2.*.Poincare.pi) -. delta_tmp);;

		(* Get all the transformation parameters *)
		let getParameters ((mI,nb_I) : Minutae.minutae array * int) ((mT,nb_T) : Minutae.minutae array * int ) =
			(* Make parameters arrays *)
			let delta_x = Array.make (nb_I*nb_T) 0.
			and delta_y = Array.make (nb_I*nb_T) 0.
			and delta_theta = Array.make (nb_I*nb_T) 0.
			and cur = ref 0 in
			for i = 0 to (nb_I-1) do
				for j = 0 to (nb_T-1) do
					(* Calculate deltas *)
					let tmp_x = abs (mT.(j).x - mI.(i).x) in
					let tmp_y = abs (mT.(j).y - mI.(i).y) in
					let tmp_theta = delta mI mT i j in
					(* Put in arrays *)
					delta_x.(!cur) <- (float_of_int tmp_x);
					delta_y.(!cur) <- (float_of_int tmp_y);
					delta_theta.(!cur) <- tmp_theta;
					(* Increment values *)
					cur := !cur + 1;
				done;
			done;
			(* Minimize delta sizes *)
			let r_compare a b = (-1) * (compare a b) in
			Array.sort r_compare delta_x;
			Array.sort r_compare delta_y;
			Array.sort r_compare delta_theta;
			(* Remove duplicates - it's worth it *)
			let rec remove_duplicates_aux arr l n error =
				if n = (nb_I*nb_T) then l
				else if arr.(n) = arr.(n-1) then remove_duplicates_aux arr l (n+1) error
				(* Set a precision on theta of 10^(-10) ~ 10^(-8)° *)
				else if abs_float (arr.(n) -. arr.(n-1)) < error then
					remove_duplicates_aux arr l (n+1) error
				else remove_duplicates_aux arr (arr.(n)::l) (n+1) error; in
			let remove_duplicates arr error = remove_duplicates_aux arr [arr.(0)] 1 error in
			let ret_delta_x = Array.of_list (remove_duplicates delta_x 4.) in
			let ret_delta_y = Array.of_list (remove_duplicates delta_y 4.) in
			let ret_delta_theta = Array.of_list (remove_duplicates delta_theta 2.8) in
			(ret_delta_x,ret_delta_y,ret_delta_theta);;
 
		(* Vote for best delta combo *)
		let voteParameters (mI,nb_I) (mT,nb_T) =
			(* Get parameters *)
			let (delta_x,delta_y,delta_theta) = getParameters (mI,nb_I) (mT,nb_T) in
			(* Create votes structure *)
			let votes = Array.init (Array.length delta_x)
									(fun _ -> Array.init (Array.length delta_y)
									(fun _ -> (Array.make (Array.length delta_theta) 0))) in 
			(* Get optimal index for an operation - TODO: Optimize *)
			let getMin delta_typ el =
				let ret = ref (abs_float (el -. delta_typ.(0))) in
				let ret_ind = ref 0 in
				for k = 1 to ((Array.length delta_typ)-1) do
					let tmp = abs_float (el -. delta_typ.(k)) in
					if tmp < !ret then
						(ret := tmp;
						ret_ind := k);
				done; !ret_ind in
			(* Find best ones *)
			for i = 0 to (nb_I-1) do
				for j = 0 to (nb_T-1) do
					(* Rotation *)
					let k_t = getMin delta_theta (delta mI mT i j) in
					let best_theta = delta_theta.(k_t) in
					(* Translation *)
					let ix = float_of_int mI.(i).x in
					let iy = float_of_int mI.(i).y in
					let jx = float_of_int mT.(j).x in
					let jy = float_of_int mT.(j).y in
					let tmp_x = jx -. ((cos best_theta) *. ix -. (sin best_theta) *. iy) in
					let tmp_y = jy -. ((sin best_theta) *. ix +. (cos best_theta) *. iy) in
					let k_x = getMin delta_x tmp_x in
					let k_y = getMin delta_y tmp_y in
					(* Insert votes *)
					votes.(k_x).(k_y).(k_t) <- votes.(k_x).(k_y).(k_t) + 1;
				done;
			done;
			(* Get best guy *)
			let cur_max_vote = ref (-1) in
			let ret = ref (-1,-1,-1) in
			for i = 0 to (Array.length delta_x)-1 do
				for j = 0 to (Array.length delta_y)-1 do
					for h = 0 to (Array.length delta_theta)-1 do
						if votes.(i).(j).(h) > !cur_max_vote then
							(cur_max_vote := votes.(i).(j).(h);
							ret := (i,j,h));
					done;
				done;
			done;
			let (x_ret,y_ret,theta_ret) = !ret in
			(delta_x.(x_ret),delta_y.(y_ret),delta_theta.(theta_ret));;

		(* Get matching score after vote *)
		let getScore (mI,nb_I) (mT,nb_T) =
			(* Accepted errors *)
			let error_x = 4 in
			let error_y = 4 in
			let error_theta = 2.8 in
			let (d_x,d_y,d_t) = voteParameters (mI,nb_I) (mT,nb_T) in
			(* Translate and Rotate mI *)
			let mI_n = Array.copy mI in
			for i = 0 to (nb_I-1) do
				let x' = float_of_int mI_n.(i).x in
				let y' = float_of_int mI_n.(i).y in
				let x_new = x' *. (cos d_t) -. y' *. (sin d_t) +. d_x in
				let y_new = x' *. (sin d_t) +. y' *. (cos d_t) +. d_y in
				let theta_new = mI_n.(i).theta +. d_t in
				mI_n.(i) <- {x = (int_of_float x_new) ; y = (int_of_float y_new); theta = theta_new};
			done;
			(* Compare both lists *)
			let n_matched = ref 0 in
			for i = 0 to (nb_I-1) do
				for j = 0 to (nb_T-1) do
					let same_x = (abs (mI_n.(i).x - mT.(j).x)) < error_x in
					let same_y = (abs (mI_n.(i).y - mT.(j).y)) < error_y in
					let same_theta = (delta mI_n mT i j) < error_theta in
					if same_x && same_y && same_theta then n_matched := !n_matched + 1;
				done;
			done;
			(* Return score *)
			((float_of_int !n_matched)**2.) /. (float_of_int (nb_I*nb_T));;

	end
