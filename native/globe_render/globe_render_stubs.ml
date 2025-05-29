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
    ~filename:
      "_build/default/native/render_globe_screen/dllrender_globe_screen_stubs.so"
    ~flags:Dl.[ RTLD_NOW; RTLD_GLOBAL ]

let render_globe_screen =
  foreign ~from:lib "render_globe_screen"
    (int @-> int @-> int @-> int @-> int @-> int @-> ptr int @-> int
   @-> ptr rgb_pixel @-> returning void)
