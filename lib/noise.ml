let patterns = [|0o25; 0o70; 0o62; 0o54; 0o15; 0o23; 0o07; 0o52|]

let btst n b = (n lsr b) land 1

let bmix i j k b = patterns.((btst i b lsl 2) lor (btst j b lsl 1) lor btst k b)

let shuffle ?(seed = 0) (i, j, k) =
  let i, j, k = (i + seed, j + seed, k + seed) in
  bmix i j k 0 + bmix j k i 1 + bmix k i j 2 + bmix i j k 3 + bmix j k i 4
  + bmix k i j 5 + bmix i j k 6 + bmix j k i 7

let magnitude ?(mag_scale = 1.) h (x, y, z) =
  let p, q, r =
    match h land 7 with
    | 0 ->
        (z, x, y)
    | 1 ->
        (x, y, 0.)
    | 2 ->
        (y, z, 0.)
    | 3 ->
        (z, x, 0.)
    | 4 ->
        (z, x, y)
    | 5 ->
        (x, 0., z)
    | 6 ->
        (y, 0., x)
    | 7 ->
        (z, 0., y)
    | _ ->
        assert false
  in
  match (h lsr 3) land 7 with
  | 0 ->
      mag_scale *. (-.p -. q +. r)
  | 1 ->
      mag_scale *. (+.p -. q -. r)
  | 2 ->
      mag_scale *. (-.p +. q -. r)
  | 3 ->
      mag_scale *. (+.p +. q +. r)
  | 4 ->
      mag_scale *. (+.p +. q -. r)
  | 5 ->
      mag_scale *. (-.p +. q +. r)
  | 6 ->
      mag_scale *. (+.p -. q +. r)
  | 7 ->
      mag_scale *. (-.p -. q -. r)
  | _ ->
      assert false

let simplices =
  [| [|(0, 0, 0); (1, 0, 0); (1, 1, 0); (1, 1, 1)|]
   ; [|(0, 0, 0); (1, 0, 0); (1, 0, 1); (1, 1, 1)|]
   ; [|(0, 0, 0); (0, 1, 0); (1, 1, 0); (1, 1, 1)|]
   ; [|(0, 0, 0); (0, 1, 0); (0, 1, 1); (1, 1, 1)|]
   ; [|(0, 0, 0); (0, 0, 1); (1, 0, 1); (1, 1, 1)|]
   ; [|(0, 0, 0); (0, 0, 1); (0, 1, 1); (1, 1, 1)|] |]

let permindex (u, v, w) =
  if u >= w then
    if u >= v then
      if v >= w then
        0
      else
        1
    else
      2
  else if v >= w then
    3
  else if u >= v then
    4
  else
    5

let int x =
  if x < 0. then
    pred (truncate x)
  else
    truncate x

let skew (x, y, z) =
  let s = (x +. y +. z) /. 3. in
  let i = int (x +. s) and j = int (y +. s) and k = int (z +. s) in
  (i, j, k)

let unskew (x, y, z) (i, j, k) =
  let s = float (i + j + k) /. 6. in
  let u = x -. float i +. s
  and v = y -. float j +. s
  and w = z -. float k +. s in
  (u, v, w)

let norm2 (x, y, z) = (x *. x) +. (y *. y) +. (z *. z)

let addi3 (i, j, k) (i', j', k') = (i + i', j + j', k + k')

let noise ?(seed = 0) ?(mag_scale = 1.) ?(tile_x_width = None) ?(x_scale = 1.)
    ?(y_scale = 1.) (x, y, z) =
  let x, y, z =
    match tile_x_width with
    | None ->
        (x, y, z)
    | Some w ->
        let theta = 2. *. Float.pi *. x /. float w in
        (Float.cos theta, y, Float.sin theta)
  in
  let x, y = (x *. x_scale, y *. y_scale) in
  let l = skew (x, y, z) in
  let x = unskew (x, y, z) l in
  let s = simplices.(permindex x) in
  let f = ref 0. in
  for i = 0 to 3 do
    let v = s.(i) in
    let y = unskew x v in
    let t = 0.6 -. norm2 y in
    if t > 0. then
      let h = shuffle ~seed (addi3 l v) in
      let t = t *. t in
      f := !f +. (8. *. t *. t *. magnitude ~mag_scale h y)
  done ;
  (!f +. 1.) /. 2.

let contrast f power =
  if f < 0.5 then
    0.5 *. ((2. *. f) ** power)
  else
    1.0 -. (0.5 *. ((2. *. (1.0 -. f)) ** power))

let fbm ?(seed = 0) ?(octaves = 5) ?(persistence = 0.5) ?(lacunarity = 2.0)
    ?(mag_scale = 1.) ?(contrast_power = 1.) ?(tile_x_width = None)
    ?(x_scale = 1.) ?(y_scale = 1.) (x, y, z) =
  let f = ref 0.0 in
  let amp = ref 1.0 in
  let freq = ref 1.0 in
  let max_amp = ref 0.0 in
  for _ = 0 to octaves - 1 do
    f :=
      !f
      +. !amp
         *. noise ~seed ~mag_scale ~tile_x_width ~x_scale ~y_scale
              (x *. !freq, y *. !freq, z *. !freq) ;
    max_amp := !max_amp +. !amp ;
    amp := !amp *. persistence ;
    freq := !freq *. lacunarity
  done ;
  let x = !f /. !max_amp in
  let x =
    if x < 0. then
      0.
    else if x > 1. then
      1.
    else
      x
  in
  contrast x contrast_power

let clampb n =
  n
  lor ((255 - n) asr (Sys.word_size - 2))
  land lnot (n asr (Sys.word_size - 2))
  land 255

let rescale f = clampb (int (0.5 +. ldexp (f +. 1.) 7))

(* let () =
  for x = 0 to 100 do
    for y = 0 to 100 do
      Printf.printf "%f %f %f\n" (float x) (float y)
        (fbm ~mag_scale:5. (float x, float y, 0.))
    done
  done *)
