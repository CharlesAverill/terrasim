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
