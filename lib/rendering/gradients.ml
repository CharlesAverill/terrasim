(** Logic for interpolating colors on a gradient *)

open World.Altitude

type gradient = string * (float * (int * int * int)) array
(** Name, array of (threshold in {m [0, 1]} and [(r, g, b)] at that threshold)
*)

(** Blue to green to brown to white *)
let seaborn_gradient : gradient =
  ( "seaborn",
    [|
      (0.0, (0, 0, 128));
      (* Deep ocean blue *)
      (0.3, (70, 130, 180));
      (* Shallow ocean/light blue *)
      (0.5, (60, 179, 113));
      (* Land green *)
      (0.7, (139, 69, 19));
      (* Mountain brown *)
      (1.0, (255, 250, 250))
      (* Snow white *);
    |] )

(** Reds and browns *)
let mars_gradient : gradient =
  ( "mars",
    [|
      (0.0, (101, 67, 33)) (* Dark brown - low elevation *);
      (0.25, (139, 69, 19)) (* Saddle brown *);
      (0.5, (160, 82, 45)) (* Sienna *);
      (0.75, (210, 105, 30)) (* Chocolate *);
      (1.0, (222, 184, 135)) (* Burlywood - light brown *);
    |] )

(** Blue to green to red *)
let bgr_gradient : gradient =
  ( "bgr",
    [|
      (0.0, (0, 0, 255)) (* Deep blue - lowest point *);
      (0.3, (0, 255, 0)) (* Green - mid elevation *);
      (1.0, (255, 0, 0)) (* Red - highest point *);
    |] )

(** Dark brown to white *)
let height_gradient : gradient =
  ( "height",
    [|
      (0.0, (74, 55, 25));
      (0.15, (115, 82, 56));
      (0.3, (136, 110, 88));
      (0.5, (157, 137, 119));
      (0.75, (178, 164, 150));
      (1.0, (192, 192, 192));
    |] )

(** Dark blue to light blue *)
let ocean_gradient : gradient =
  ( "ocean",
    [|
      (0.0, (13, 65, 225));
      (float deep_ocean_theshold, (10, 133, 237));
      (float regular_ocean_theshold, (7, 200, 249));
      (float shallow_ocean_theshold, (7, 200, 249));
    |] )

(** A cache for gradient results *)
let gradient_cache : (string * int, int * int * int) Hashtbl.t =
  Hashtbl.create 512

(** Get a color on [gradient] at [v]
    @param gradient The gradient to sample from
    @param v Position on gradient in {m [0, 1]} to sample at
    @return The gradient sample in RGB *)
let interpolate_gradient (gradient : gradient) (v : float) : int * int * int =
  let quantize v = int_of_float (v *. 1000.) in
  let key = (fst gradient, quantize v) in
  match Hashtbl.find_opt gradient_cache key with
  | Some color ->
      color
  | None ->
      let gradient = snd gradient in
      (* Original interpolation *)
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
      let v1, (r1, g1, b1) =
        gradient.(min (i + 1) (Array.length gradient - 1))
      in
      let t =
        if v1 = v0 then
          0.0
        else
          (v -. v0) /. (v1 -. v0)
      in
      let lerp a b = int_of_float (float a +. (t *. float (b - a))) in
      let result = (lerp r0 r1, lerp g0 g1, lerp b0 b1) in
      Hashtbl.add gradient_cache key result;
      result
