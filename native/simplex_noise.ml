open Ctypes
open Foreign

(* 
struct osn_context {
	int16_t *perm;
	int16_t *permGradIndex3D;
};
*)

type osn_context

let osn_context : osn_context Ctypes.structure typ = structure "osn_context"
let perm = field osn_context "perm" (ptr uint16_t)
let perm_grad_index_3d = field osn_context "permGradIndex3D" (ptr uint16_t)
let () = seal osn_context

let lib =
  Dl.dlopen ~filename:"_build/default/native/dllsimplex_noise_stubs.so"
    ~flags:Dl.[ RTLD_NOW; RTLD_GLOBAL ]

let init_open_simplex_noise =
  foreign ~from:lib "open_simplex_noise"
    (int64_t @-> ptr (ptr osn_context) @-> returning int)

let open_simplex_noise_2d =
  foreign ~from:lib "open_simplex_noise2"
    (ptr osn_context @-> double @-> double @-> returning double)

let open_simplex_noise_3d =
  foreign ~from:lib "open_simplex_noise4"
    (ptr (const osn_context)
    @-> double @-> double @-> double @-> returning double)

let open_simplex_noise_4d =
  foreign ~from:lib "open_simplex_noise4"
    (ptr (const osn_context)
    @-> double @-> double @-> double @-> double @-> returning double)

let destroy_osn_context =
  foreign ~from:lib "open_simplex_noise_free"
    (ptr osn_context @-> returning void)

let create_osn_context (seed : int64) : osn_context structure ptr =
  (* Allocate space for a pointer to osn_context *)
  let ctx_ptr_ptr = allocate (ptr osn_context) (from_voidp osn_context null) in

  (* Call the initializer *)
  let result = init_open_simplex_noise seed ctx_ptr_ptr in
  if result <> 0 then
    failwith ("open_simplex_noise failed with code " ^ string_of_int result);

  !@ctx_ptr_ptr
