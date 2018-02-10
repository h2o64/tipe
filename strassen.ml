(*
	Project Name: Strassen's Matrix multiplicaion
	Project Members: 
	Lijo Johny (IMT2012023)
	Roniit Bhimrajka (IMT2012038)

	Adapted for Bigarray.Array2 by Louis Popi
*)


(* Set FFT Module *)
module FFT = Fftw3.D;;

module Strassen :
  sig
		val make_matrix :
			int ->
			int -> (Complex.t, FFT.complex_elt, Bigarray.c_layout) Bigarray.Array2.t
		val matrix_add :
			int ->
			(Complex.t, 'a, 'b) Bigarray.Array2.t ->
			(Complex.t, 'c, 'd) Bigarray.Array2.t ->
			(Complex.t, FFT.complex_elt, Bigarray.c_layout) Bigarray.Array2.t
		val matrix_sub :
			int ->
			(Complex.t, 'a, 'b) Bigarray.Array2.t ->
			(Complex.t, 'c, 'd) Bigarray.Array2.t ->
			(Complex.t, FFT.complex_elt, Bigarray.c_layout) Bigarray.Array2.t
		val matrix_mult :
			int ->
			(Complex.t, FFT.complex_elt, Bigarray.c_layout) Bigarray.Array2.t ->
			(Complex.t, FFT.complex_elt, Bigarray.c_layout) Bigarray.Array2.t ->
			(Complex.t, FFT.complex_elt, Bigarray.c_layout) Bigarray.Array2.t
		val pad :
			int ->
			(Complex.t, FFT.complex_elt, Bigarray.c_layout) Bigarray.Array2.t ->
			(Complex.t, FFT.complex_elt, Bigarray.c_layout) Bigarray.Array2.t ->
			(Complex.t, FFT.complex_elt, Bigarray.c_layout) Bigarray.Array2.t
  end =
  struct

		(* A function to get a complex zero-ed NxN Bigarray *)
		let make_matrix a b =
			let ret = FFT.Array2.create FFT.complex Bigarray.c_layout a b in
			Bigarray.Array2.fill ret Complex.zero;
			ret;;

		(* A function to add two matrices of size n
		*)
		let matrix_add n a b =
			let m = make_matrix n n in
			for i = 0 to n-1 do
				for j = 0 to n-1 do
					m.{i,j} <- Complex.add a.{i,j}  b.{i,j}
				done
			done;
			m;;

		(* A function to subtract two matrices of size n
		*)
		let matrix_sub n a b =
			let p = make_matrix n n in
			for i = 0 to n-1 do
				for j = 0 to n-1 do
					p.{i,j} <- Complex.sub a.{i,j} b.{i,j}
				done
			done;
			p;;


		let rec matrix_mult n x y =
		(*
		Matrix multiplication for matrix of size 2 or less 
		*)
			if (n <= 2) then
				let z = make_matrix n n in
				for i = 0 to n-1 do
					for j = 0 to n-1 do
						for k = 0 to n-1 do
						z.{i,j} <- Complex.add z.{i,j} (Complex.mul x.{i,k} y.{k,j})
						done
					done
				done;
				z
		(*
		Matrix multiplication for matrix of size 3 or more 
		*)
			else
				let t = n/2 in
		(*
		Creation of 4 matrix of size n/2 of matrix x
		*)
				let a11 = make_matrix t t in
				for i = 0 to t-1 do
					for j = 0 to t-1 do
						a11.{i,j} <- x.{i,j}
					done
				done;
				let a12 = make_matrix t t in
				for i = 0 to t-1 do
					for j = 0 to t-1 do
						a12.{i,j} <- x.{i,j+t}
					done
				done;
				let a21 = make_matrix t t in
				for i = 0 to t-1 do
					for j = 0 to t-1 do
						a21.{i,j} <- x.{i+t,j}
					done
				done;
				
				let a22 = make_matrix t t in
				for i = 0 to t-1 do
					for j = 0 to t-1 do
						a22.{i,j} <- x.{i+t,j+t}
					done
				done;
		(* Creation of 4 matrix of size n/2 of matrix y
		*)
				let b11 = make_matrix t t in
				for i = 0 to t-1 do
					for j = 0 to t-1 do
						b11.{i,j} <- y.{i,j}
					done
				done;
				let b12 = make_matrix t t in
				for i = 0 to t-1 do
					for j = 0 to t-1 do
						b12.{i,j} <- y.{i,j+t}
					done
				done;
				let b21 = make_matrix t t in
				for i = 0 to t-1 do
					for j = 0 to t-1 do
						b21.{i,j} <- y.{i+t,j}
					done
				done;
				let b22 = make_matrix t t in
				for i = 0 to t-1 do
					for j = 0 to t-1 do
						b22.{i,j} <- y.{i+t,j+t}
					done
				done;
		(*
		Calculattion of s1-s10 		
		*)
				let s1 = matrix_sub t b12 b22
				and s2 = matrix_add t a11 a12 
				and s3 = matrix_add t a21 a22 
				and s4 = matrix_sub t b21 b11 
				and s5 = matrix_add t a11 a22 
				and s6 = matrix_add t b11 b22 
				and s7 = matrix_sub t a12 a22 
				and s8 = matrix_add t b21 b22 
				and s9 = matrix_sub t a11 a21 
				and s10 = matrix_add t b11 b12 in
		(*
		Calculattion of p1-p7 which are recursive		
		*)
				let p1=matrix_mult t a11 s1 
				and p2=matrix_mult t s2 b22 
				and p3=matrix_mult t s3 b11 
				and p4=matrix_mult t a22 s4 
				and p5=matrix_mult t s5 s6 
				and p6=matrix_mult t s7 s8 
				and p7=matrix_mult t s9 s10 in

				let k1= matrix_add t p5 p4 in
				let k2= matrix_sub t k1 p2 in
				let k3 = matrix_add t k2 p6 in
				let k4 = matrix_add t p1 p2 
				and k5 = matrix_add t p3 p4 in
				let k6 = matrix_add t p5 p1 in
				let k7 = matrix_sub t k6 p3 in
				let k8 = matrix_sub t k7 p7 in
				
				let temp1 = make_matrix t t in
				for i = 0 to t-1 do
					for j = 0 to t-1 do
						temp1.{i,j} <- k3.{i,j}
					done
				done;
			
				let temp2 = make_matrix t t in
				for i = 0 to t-1 do
					for j = 0 to t-1 do
						temp2.{i,j} <- k4.{i,j}
					done
				done;
				
				let	temp3 = make_matrix t t in
				for i = 0 to t-1 do
					for j = 0 to t-1 do
						temp3.{i,j} <- k5.{i,j}
					done
				done;
				
				let temp4 = make_matrix t t in
				for i = 0 to t-1 do
					for j = 0 to t-1 do
						temp4.{i,j} <- k8.{i,j}
					done
				done; 
		(*
		Joining of all 4 matrices to form the required output		
		*)
				let temp = make_matrix n n in
				for i = 0 to t-1 do
					for j = 0 to t-1 do
						temp.{i,j} <- temp1.{i,j};
						temp.{i,j+t} <- temp2.{i,j};
						temp.{i+t,j} <- temp3.{i,j};
						temp.{i+t,j+t} <- temp4.{i,j}
					done
				done;
			temp;;

		(*
		Function to pad the matrix such that n is power of 2
		*)
		let pad n a b =
			if log(float_of_int(n))/. log(2.0) -. float_of_int(int_of_float(log(float_of_int(n))/. log(2.0))) >0.0
			then
				let k = int_of_float(2.0 ** float_of_int(int_of_float(log(float_of_int(n))/. log(2.0))+1) ) in
				let t1 = make_matrix k k and		
				t2 = make_matrix k k in
				for i = 0 to n-1 do
					for j = 0 to n-1 do
						t1.{i,j} <- a.{i,j};
						t2.{i,j} <- b.{i,j}
					done
				done;
				matrix_mult k t1 t2 
				
			else
				matrix_mult n a b;;
	end
