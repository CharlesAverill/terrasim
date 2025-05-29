(** Simulation step logic *)

open Simulation_info
open Z

(** Run one timestep of the simulation *)
let simulate () = sim_year := !sim_year + one
