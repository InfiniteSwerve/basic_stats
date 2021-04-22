open Base


(* Main *)
module Basic_stats = struct
  

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
  Alcotest.(check @@ float e) "median single list 5" 5. (Basic_stats.median [5]);

  Alcotest.(check @@ float e) "same median, odd" 2. (Basic_stats.median [1;2;3]);
  Alcotest.(check @@ float e) "same median, even" 2.5 (Basic_stats.median [1;2;3;4])



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
    ]