(** Simplex noise generator and FBM

    https://github.com/smcameron/open-simplex-noise-in-c *)

open Simplex_noise
open Utils.Standard_utils

let ctx = ref (Ctypes.addr (Ctypes.make osn_context))
let shut_down_noise_gen () = destroy_osn_context !ctx

let init_noise_gen ?(seed : int option = None) () =
  let seed =
    match seed with
    | Some x ->
        x
    | None ->
        Random.self_init ();
        Random.int 4096
  in
  ctx := create_osn_context (Int64.of_int seed)

(** A single octave of 4-dimensional simplex noise, sampled on two circles.
    Tiles on [w] and [h]

    @param scale Zoom factor (radius of circles sampled from)
    @param ctx Should be {!ctx}, initialize via {!init_noise_gen}
    @param x
    @param y
    @param w Tile width
    @param h Tile height *)
let noise_2d ?(scale : float = 1.) ctx (x, y) (w, h) frequency amplitude : float
    =
  let two_pi = 2. *. Float.pi in
  let s = two_pi *. float x /. float w in
  let t = two_pi *. float y /. float h in
  let nx = scale *. Float.cos s in
  let ny = scale *. Float.cos t in
  let nz = scale *. Float.sin s in
  let nw = scale *. Float.sin t in

  amplitude
  *. open_simplex_noise_4d ctx (frequency *. nx) (frequency *. ny)
       (frequency *. nz) (frequency *. nw)

(** Fractal Brownian Motion using tiled 4D simplex noise on two circles.

    @param ?scale Base scale (circle radius)
    @param ?octaves Number of noise octaves
    @param ?persistence Amplitude decay factor per octave
    @param ?lacunarity Frequency growth factor per octave
    @param x
    @param y
    @param w Tile width (wraps in X)
    @param h Tile height (wraps in Y)
    @return Noise value in {m [0, 1]} *)
let fbm_2d ?(scale_xy : float * float = (1., 1.)) ?(octaves = 4)
    ?(persistence = 0.5) ?(lacunarity = 2.0) ?(base_freq : float = 1.0)
    ?(base_ampl : float = 1.0) ?(contrast : float = 1.) (x, y) (w, h) : float =
  let scale_x, scale_y = scale_xy in
  let two_pi = 2. *. Float.pi in
  let s = two_pi *. float x /. float w in
  let t = two_pi *. float y /. float h in
  let nx = scale_y *. Float.cos s in
  let ny = scale_x *. Float.cos t in
  let nz = scale_y *. Float.sin s in
  let nw = scale_x *. Float.sin t in

  let n = ref 0. in
  let amplitude = ref base_ampl in
  let frequency = ref base_freq in
  for i = 0 to octaves do
    n :=
      !n
      +. !amplitude
         *. open_simplex_noise_4d !ctx (!frequency *. nx) (!frequency *. ny)
              (!frequency *. nz) (!frequency *. nw);
    amplitude := !amplitude *. persistence;
    frequency := !frequency *. lacunarity
  done;

  let n = (clamp !n (-1.) 1. +. 1.) /. 2. in
  Float.pow n contrast
