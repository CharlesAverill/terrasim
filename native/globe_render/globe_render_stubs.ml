open Ctypes
open Foreign

type rgb_pixel

let rgb_pixel : rgb_pixel Ctypes.structure typ = structure "RGBPixel"

let r = field rgb_pixel "r" uint8_t

let g = field rgb_pixel "g" uint8_t

let b = field rgb_pixel "b" uint8_t

let () = seal rgb_pixel

let lib =
  Dl.dlopen
    ~filename:"_build/default/native/globe_render/dllglobe_render_stubs.so"
    ~flags:Dl.[RTLD_NOW; RTLD_GLOBAL]

let globe_render =
  foreign ~from:lib "globe_render"
    ( int @-> int @-> int @-> int @-> int @-> int @-> ptr int @-> int
    @-> ptr rgb_pixel @-> returning void )
