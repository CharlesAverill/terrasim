(** OpenGL program to draw the globe screen *)

open Tsdl
open Tgl4
open Utils.Logging
open Utils.Opengl_utils
open Globe_data

(** The
    {{:https://github.com/CharlesAverill/terrasim/blob/main/shaders/globe.vert}
     globe vertex shader} *)
let globe_vertex_shader_src = [%blob "shaders/globe.vert"]

(** The
    {{:https://github.com/CharlesAverill/terrasim/blob/main/shaders/globe.frag}
     globe fragment shader} *)
let globe_fragment_shader_src = [%blob "shaders/globe.frag"]

(** Shader program compiled for the current OpenGL context, or [None] *)
let sprogram = ref None

(** Cached atlas texture, or [None] *)
let atlastex = ref None

(** Render the globe screen
    @param window Application's SDL window
    @param frame_counter The current frame counter *)
let render_globe_screen (window : Sdl.window) (frame_counter : int) =
  let sprogram =
    match !sprogram with
    | None ->
        let x =
          compile_shaders globe_vertex_shader_src globe_fragment_shader_src
        in
        sprogram := Some x;
        x
    | Some x ->
        x
  in
  let quad_vao = setup_fullscreen_quad () in
  (* Render atlas to texture *)
  let atlas_texture =
    if !Atlas_screen_data.atlas_view_shifted then atlastex := None;
    match !atlastex with
    | Some a ->
        a
    | None -> (
        match Atlas.render_atlas_screen ~to_texture:true window with
        | None ->
            fatal rc_OpenGL
              "Globe renderer didn't get FBO and atlas texture from atlas \
               renderer"
        | Some a ->
            atlastex := Some a;
            a)
  in
  (* Bind default framebuffer to render globe to screen *)
  Gl.bind_framebuffer Gl.framebuffer 0;
  let win_w, win_h = Sdl.get_window_size window in
  Gl.viewport 0 0 win_w win_h;
  Gl.clear_color 0.0 0.0 0.0 1.0;
  Gl.clear Gl.color_buffer_bit;
  (* Use globe shader *)
  Gl.use_program sprogram;
  Gl.bind_vertex_array quad_vao;
  (* Set iResolution uniform *)
  let u_resolution = Gl.get_uniform_location sprogram "iResolution" in
  Gl.uniform2f u_resolution (float win_w) (float win_h);
  let u_time = Gl.get_uniform_location sprogram "iTime" in
  Gl.uniform1i u_time frame_counter;
  let u_time = Gl.get_uniform_location sprogram "iLat" in
  Gl.uniform1f u_time !rotation_lat;
  let u_time = Gl.get_uniform_location sprogram "iLon" in
  Gl.uniform1f u_time !rotation_lon;
  (* Bind atlas texture to iChannel0 = texture unit 0 *)
  Gl.active_texture Gl.texture0;
  Gl.bind_texture Gl.texture_2d atlas_texture;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  let u_ichannel0 = Gl.get_uniform_location sprogram "iChannel0" in
  Gl.uniform1i u_ichannel0 0;
  (* Draw *)
  Gl.draw_arrays Gl.triangles 0 6;
  (* Swap *)
  Sdl.gl_swap_window window
