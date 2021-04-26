open Base


(* Main *)
module Basic_stats = struct
  
let rec float_sum l accum = 
  match l with
  | [] -> accum
  | hd::tl -> float_sum tl (accum +. hd)
let rec sum l accum  = 
  match l with 
  | [] -> accum
  | hd::tl -> sum tl (accum+hd)

let mean data = 
  if List.length data = 0 then invalid_arg "list must be nonempty for mean"
  else let sm = Int.to_float(sum data 0) in
  (sm /. Int.to_float(List.length data))


let median data = 
  let data = List.sort data ~compare:compare in
  let len = List.length data in
  if len%2 = 0 then mean [List.nth_exn data (len/2-1); List.nth_exn data (len/2)] 
  else mean [List.nth_exn data ((len-1)/2)]


(** Counts occurences of each num in list l and creates association list *)
let number_counts l = 
  List.fold l ~init:[] ~f:(fun counts num ->
    let count = 
      match List.Assoc.find ~equal:Int.equal counts num with
      | None -> 0
      | Some x -> x
    in
    List.Assoc.add ~equal:Int.equal counts num (count + 1))
    

(** Returns the key corresponding to the max value in an association list *)
let rec max_val assoc_list k v = 
  match assoc_list with
  | [] -> k
  | (k',v')::tl -> if v' > v then max_val assoc_list k' v' else max_val tl k v

(** Returns mode of data. If multiple valid modes will return the last one*)
let mode data = 
  let n = number_counts data in
  max_val n 0 0 

(** Returns the last item in a list, raises error on an empty list *)
let rec last = function
  | [] -> invalid_arg "last needs nonempty list"
  | [l] -> l
  | _::tl -> last tl

(** Extracts the first n items in a list. Returns entire list if n is greater than list length *)
let rec extract n = function
  | [] -> []
  | hd::tl -> if n > 0 then hd::(extract (n-1) tl) 
  else []


(** Extracts indices from i to j, zero indexed.*)
let rec slice l i j = 
  if (i>=j || (List.length l = 0)) 
    then invalid_arg "invalid indices or empty list"
  else 
  match l with
  | [] -> []
  | hd::tl -> if i > 0 then slice tl (i-1) (j-1)
  else hd::(extract (j-i) tl)

(** Returns variance of data. Optional boolean argument for 
whether data is sample or population*)
let variance ?(sample=false) data = 
  let mn = mean data in
  let diffs = List.map data ~f:(fun x -> 
    ((Int.to_float x) -. mn) **. 2. ) in
  if sample then (float_sum diffs 0.) /. Int.to_float((List.length data)-1)
  else (float_sum diffs 0.) /. Int.to_float(List.length data)


(** Returns variance of data. Optional boolean argument for
whether data is sample or population*)
let standard_deviation ?(sample=false) data = 
  (variance ~sample:sample data) **. 0.5  

(** Non tail-recursive factorial function *)
let rec factorial n =
  if n=0 then 1 
  else n*(factorial (n-1))

(** Non tail-recursive permutation function*)
let rec permutation n k = 
  if k=0 then 1
  else n*(permutation (n-1) (k-1))


(** Non tail-recursive combination function*)
let combination n k = 
  (permutation n k) / (factorial (k))

end
(* Tests *)  




(* I don't know why, but I need this e to make float tests work*)
let e = Float.epsilon_float
let test_sum () = 
  Alcotest.(check int) "same sum" 10 (Basic_stats.sum [1;2;3;4] 0);
  Alcotest.(check int) "same sum" 500_500 (Basic_stats.sum (List.range 1 1001) 0)

let test_mean () = 
  Alcotest.(check @@ float e) "same mean" 1. (Basic_stats.mean [1]);
  Alcotest.(check @@ float e) "same mean" 500.5 (Basic_stats.mean (List.range 1 1001))  

let test_median () =
  Alcotest.(check @@ float e) "median single list 1" 1. (Basic_stats.median [1]);
  Alcotest.(check @@ float e) "same median, odd" 2. (Basic_stats.median [1;2;3]);
  Alcotest.(check @@ float e) "same median, even" 2.5 (Basic_stats.median [1;2;3;4])

let test_number_counts () = 
  Alcotest.(check (list (pair int  int))) "singleton list" [(1,1)] (Basic_stats.number_counts [1]);
  Alcotest.(check (list (pair int  int))) "varied list" [(4,1);(3,1);(2,1);(1,1)] (Basic_stats.number_counts [1;2;3;4]);
  Alcotest.(check (list (pair int  int))) "long singleton list" [(1,12)] (Basic_stats.number_counts [1;1;1;1;1;1;1;1;1;1;1;1])

let test_max_val () = 
  Alcotest.(check int) "singleton list" 1 (Basic_stats.max_val [(1,1)] 0 0 );
  Alcotest.(check int) "pair list" 2 (Basic_stats.max_val [(1,1);(2,3)] 0 0 );
  Alcotest.(check int) "several options list" 1 (Basic_stats.max_val [(1,1);(2,1)] 0 0 )

let test_mode () = 
  Alcotest.(check int) "single list mode" 1 (Basic_stats.mode [1]);
  Alcotest.(check int) "multiple valid modes" 4 (Basic_stats.mode [1;2;3;4]);
  Alcotest.(check int) "standard mode test" 1 (Basic_stats.mode [1;2;3;1;2;1;1]);
  Alcotest.(check int) "multiple large valid modes" 2 (Basic_stats.mode [1;2;3;2;1;2;2;3;1])

let test_last () = 
(* not sure how to get empty list testcase to work #TODO*)  
(*  Alcotest.(check (invalid_arg "last needs nonempty list")) "empty list last" "last needs nonempty list" (Basic_stats.last []);*)
  Alcotest.(check int) "nonempty list last" 1 (Basic_stats.last [1]);
  Alcotest.(check int) "long nonempty list last" 10 (Basic_stats.last (List.range 1 11))

let test_extract () = 
  Alcotest.(check (list int)) "1 item extract" [1] (Basic_stats.extract 1 (List.range 1 5));
  Alcotest.(check (list int)) "2 item extract" [1;2] (Basic_stats.extract 2 (List.range 1 5));
  Alcotest.(check (list int)) "5 item extract" [1;2;3;4;5] (Basic_stats.extract 5 (List.range 1 11))

let test_slice () = 
  Alcotest.(check (list int)) "2 item slice" [1;2] (Basic_stats.slice (List.range 1 11) 0 1);
  Alcotest.(check (list int)) "7 item front slice" [1;2;3;4;5;6;7] (Basic_stats.slice (List.range 1 11) 0 6);
  Alcotest.(check (list int)) "middle 2 item slice" [2;3] (Basic_stats.slice (List.range 1 4) 1 2);
  Alcotest.(check (list int)) "rear 4 item slice" [3;4;5;6] (Basic_stats.slice (List.range 1 7) 2 6)

let test_variance () = 
  Alcotest.(check @@ float e) "basic variance of population" 2. (Basic_stats.variance [1;2;3;4;5] );
  Alcotest.(check @@ float e) "basic variance of sample" 2.5 (Basic_stats.variance ~sample:true [1;2;3;4;5] )

let test_standard_deviation () = 
  Alcotest.(check bool) "standard deviation of population" true (Poly.(1.41 < (Basic_stats.standard_deviation [1;2;3;4;5]) && (Basic_stats.standard_deviation [1;2;3;4;5]) < 1.42));
  Alcotest.(check bool) "standard deviation of sample" true Poly.(1.58 < Basic_stats.standard_deviation ~sample:true [1;2;3;4;5] && (Basic_stats.standard_deviation [1;2;3;4;5]) < 1.59)

let test_factorial () = 
  Alcotest.(check int) "unit factorial" 1 (Basic_stats.factorial 1);
  Alcotest.(check int) "5 factorial" 120 (Basic_stats.factorial 5);
  Alcotest.(check int) "10 factorial" 3628800 (Basic_stats.factorial 10)

let test_permutation () = 
  Alcotest.(check int) "unit permutation" 1 (Basic_stats.permutation 1 0);
  Alcotest.(check int) "basic permutation" 90 (Basic_stats.permutation 10 2);
  Alcotest.(check int) "big permutation" 665280 (Basic_stats.permutation 12 6)

let test_combination () = 
  Alcotest.(check int) "unit combination" 1 (Basic_stats.combination 1 0);
  Alcotest.(check int) "basic combination" 45 (Basic_stats.combination 10 2);
  Alcotest.(check int) "basic combination" 924 (Basic_stats.combination 12 6)



(* Running Tests*)
let () = 
  let open Alcotest in
    run "Utils" [
        "sum_case", [
          test_case "sum"       `Quick test_sum;
        ];
        "mean_case",[
          test_case "mean"      `Quick test_mean;
        ];
        "median_case",[
          test_case "median"    `Quick test_median;  
        ];
        "counts_case",[
          test_case "number_counts" `Quick test_number_counts;
        ];
        "max_case",[
          test_case "max_val"   `Quick test_max_val;
        ];
        "mode_case",[
          test_case "mode"      `Quick test_mode;
        ];
        "last_case",[
          test_case "last"      `Quick test_last;
        ];
        "extract_case",[
          test_case "extract"   `Quick test_extract;
        ];
        "slice_case",[
          test_case "slice"     `Quick test_slice;
        ];
        "variance_case",[      
          test_case "variance"  `Quick test_variance;
        ];
        "standard_deviation_case",[
          test_case "standard_deviation" `Quick test_standard_deviation;
        ];
        "factorial_test",[      
          test_case "factorial" `Quick test_factorial;
        ];
        "permutation_test",[
          test_case "permutation" `Quick test_permutation;
        ];
        "combination_test",[
          test_case "combination" `Quick test_combination;
        ]

]