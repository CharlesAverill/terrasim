(* Seaborn-inspired gradient: from blue to green to brown to white *)
let height_gradient =
  [| (0.0, (0, 0, 128))
   ; (* Deep ocean blue *)
     (0.3, (70, 130, 180))
   ; (* Shallow ocean/light blue *)
     (0.5, (60, 179, 113))
   ; (* Land green *)
     (0.7, (139, 69, 19))
   ; (* Mountain brown *)
     (1.0, (255, 250, 250))
     (* Snow white *) |]

(* Normalize and interpolate color *)
let interpolate_gradient gradient v =
  let rec find_segment i =
    if i + 1 >= Array.length gradient then
      i
    else if v <= fst gradient.(i + 1) then
      i
    else
      find_segment (i + 1)
  in
  let i = find_segment 0 in
  let v0, (r0, g0, b0) = gradient.(i) in
  let v1, (r1, g1, b1) = gradient.(min (i + 1) (Array.length gradient - 1)) in
  let t =
    if v1 = v0 then
      0.0
    else
      (v -. v0) /. (v1 -. v0)
  in
  let lerp a b = int_of_float (float a +. (t *. float (b - a))) in
  (lerp r0 r1, lerp g0 g1, lerp b0 b1)
