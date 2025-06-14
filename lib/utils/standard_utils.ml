(** Standard utilities *)

open Tsdl
open Logging

(** Clamp a numeric value to \[[min, max]\]
    @param x The value to clamp
    @param min
    @param max
    @return The clamped value *)
let clamp (x : 'a) (min : 'a) (max : 'a) =
  if x < min then
    min
  else if x > max then
    max
  else
    x

(** Perform a safe modulo operation on a possibly-negative number by wrapping
    @param i Left modulo operand
    @param max Right modulo operand
    @return The safely modulo'd number *)
let mod_posneg i max = ((i mod max) + max) mod max

(** Finds the first occurrence of a substring in a string.

    @param s The string to search in.
    @param sub The substring to search for.
    @return [Some index] of the first occurrence or [None] if not found. *)
let find (s : string) (sub : string) : int option =
  let s_len = String.length s in
  let sub_len = String.length sub in
  if sub_len = 0 then
    Some 0
  else
    let rec aux idx =
      if idx > s_len - sub_len then
        None
      else if Stdlib.String.sub s idx sub_len = sub then
        Some idx
      else
        aux (idx + 1)
    in
    aux 0

(** Checks if a string contains a given substring.

    @param s The string to check.
    @param sub The substring to search for.
    @return [true] if [sub] is found in [s], [false] otherwise. *)
let contains (s : string) (sub : string) : bool = find s sub != None

(** Get the width and height of the edit window for tiles and the height of the
    edit UI window for the UI
    @param window Application's SDL window
    @return Tuple of window width and window height and UI window height *)
let get_window_ui_w_h (window : Sdl.window) : int * (int * int) =
  let win_w, win_h = Sdl.get_window_size window in
  let wh =
    if !Globals.ui_window_open then
      0.85
    else
      1.
  in
  ( win_w,
    (int_of_float (float win_h *. wh), int_of_float (float win_h *. (1. -. wh)))
  )
