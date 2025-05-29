(** Information for the simulator *)

type big_z = Z.t

(** Current simulator year *)
let sim_year : big_z ref = ref (Z.of_int 0)

(** Get the simulation year as an [int] *)
let get_sim_year () : int = Z.to_int !sim_year
