(** Standard utilities *)

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
