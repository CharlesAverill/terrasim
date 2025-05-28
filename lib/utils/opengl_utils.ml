open Tgl4

let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

let get_int f : int =
  let a = bigarray_create Bigarray.int32 1 in
  f a ;
  Int32.to_int a.{0}

let get_string len f =
  let a = bigarray_create Bigarray.char len in
  f a ; Gl.string_of_bigarray a

let set_3d ba i x y z =
  let start = i * 3 in
  ba.{start} <- x ;
  ba.{start + 1} <- y ;
  ba.{start + 2} <- z

let verts_of_list (verts : (float * float * float) list) =
  let vs = bigarray_create Bigarray.float32 (3 * List.length verts) in
  List.iteri (fun i (x, y, z) -> set_3d vs i x y z) verts ;
  vs

module Mat4 = struct
  type t = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

  let create () = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 16

  let identity =
    let m = create () in
    Bigarray.Array1.fill m 0.0 ;
    m.{0} <- 1.0 ;
    m.{5} <- 1.0 ;
    m.{10} <- 1.0 ;
    m.{15} <- 1.0 ;
    m

  let mul a b =
    let res = create () in
    for i = 0 to 3 do
      for j = 0 to 3 do
        let sum = ref 0.0 in
        for k = 0 to 3 do
          sum := !sum +. (a.{(i * 4) + k} *. b.{(k * 4) + j})
        done ;
        res.{(i * 4) + j} <- !sum
      done
    done ;
    res

  let init m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33 =
    let m = create () in
    m.{0} <- m00 ;
    m.{1} <- m01 ;
    m.{2} <- m02 ;
    m.{3} <- m03 ;
    m.{4} <- m10 ;
    m.{5} <- m11 ;
    m.{6} <- m12 ;
    m.{7} <- m13 ;
    m.{8} <- m20 ;
    m.{9} <- m21 ;
    m.{10} <- m22 ;
    m.{11} <- m23 ;
    m.{12} <- m30 ;
    m.{13} <- m31 ;
    m.{14} <- m32 ;
    m.{15} <- m33 ;
    m
end

let normalize (x, y, z) =
  let len = sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
  (x /. len, y /. len, z /. len)

let rotate angle axis m =
  let x, y, z = normalize axis in
  let c = cos angle and s = sin angle in
  let c' = 1.0 -. c in
  let r =
    Mat4.init
      ((x *. x *. c') +. c)
      ((x *. y *. c') -. (z *. s))
      ((x *. z *. c') +. (y *. s))
      0.0
      ((y *. x *. c') +. (z *. s))
      ((y *. y *. c') +. c)
      ((y *. z *. c') -. (x *. s))
      0.0
      ((x *. z *. c') -. (y *. s))
      ((y *. z *. c') +. (x *. s))
      ((z *. z *. c') +. c)
      0.0 0.0 0.0 0.0 1.0
  in
  Mat4.mul r m

let sub3 (x, y, z) (a, b, c) = (x -. a, y -. b, z -. c)

let cross (a, b, c) (d, e, f) =
  ((b *. f) -. (c *. e), (c *. d) -. (a *. f), (a *. e) -. (b *. d))

let scalarmul (a, b, c) x = (a *. x, b *. x, c *. x)

let look_at (eye : float * float * float) (center : float * float * float)
    (up : float * float * float) =
  let f =
    let x, y, z = normalize (sub3 center eye) in
    (x, y, z)
  in
  let s =
    normalize
      (let a, b, c = cross f up in
       (a, b, c) )
  in
  let u = cross s f in
  let s0, s1, s2 = s in
  let u0, u1, u2 = u in
  let f0, f1, f2 = f in
  let t =
    Mat4.init s0 u0 (-.f0) 0.0 s1 u1 (-.f1) 0.0 s2 u2 (-.f2) 0.0 0.0 0.0 0.0 1.0
  in
  let eye0, eye1, eye2 = eye in
  let trans =
    Mat4.init 1.0 0.0 0.0 (-.eye0) 0.0 1.0 0.0 (-.eye1) 0.0 0.0 1.0 (-.eye2) 0.0
      0.0 0.0 1.0
  in
  Mat4.mul t trans

let perspective fovy aspect near far =
  let f = 1.0 /. tan (fovy /. 2.0) in
  let nf = 1.0 /. (near -. far) in
  Mat4.init (f /. aspect) 0.0 0.0 0.0 0.0 f 0.0 0.0 0.0 0.0
    ((far +. near) *. nf)
    (-1.0) 0.0 0.0
    (2.0 *. far *. near *. nf)
    0.0
