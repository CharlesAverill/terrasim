(** RGB color definitions *)

open Sdl_utils

let color_black = rgb_of_hex "000000"
let ui_bg_color = rgb_of_hex "DDDDDD"
let ui_bevel_light_color = rgb_of_hex "EEEEEE"
let ui_bevel_dark_color = rgb_of_hex "CCCCCC"
let black_color = rgb_of_hex "000000"
let select_highlight_color = rgb_of_hex "FFDE64"

(** Lifeform sprite transparency color *)
let lifeform_colorkey_rgb = rgb_of_hex "20C8F8"
