open Tsdl
open Sdl
open Tgl4
open Logging
open Opengl_utils
open Globals

(* Blob in your shaders *)
let globe_vertex_shader_src = [%blob "shaders/globe.vert"]

let globe_fragment_shader_src = [%blob "shaders/globe.frag"]

let quad_vertices =
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    [| (* quad verts and UVs *)
       -1.0
     ; -1.0
     ; 0.0
     ; 0.0
     ; 1.0
     ; -1.0
     ; 1.0
     ; 0.0
     ; 1.0
     ; 1.0
     ; 1.0
     ; 1.0
     ; -1.0
     ; -1.0
     ; 0.0
     ; 0.0
     ; 1.0
     ; 1.0
     ; 1.0
     ; 1.0
     ; -1.0
     ; 1.0
     ; 0.0
     ; 1.0 |]

let compile_shaders () =
  let compile_shader kind src =
    let shader = Gl.create_shader kind in
    Gl.shader_source shader src ;
    Gl.compile_shader shader ;
    let status = get_int (Gl.get_shaderiv shader Gl.compile_status) in
    ( if status = Gl.false_ then
        let len = get_int (Gl.get_shaderiv shader Gl.info_log_length) in
        let log = get_string len (Gl.get_shader_info_log shader len None) in
        fatal rc_OpenGL "Shader compile error: %s" log ) ;
    shader
  in
  let vshader = compile_shader Gl.vertex_shader globe_vertex_shader_src in
  let fshader = compile_shader Gl.fragment_shader globe_fragment_shader_src in
  let program = Gl.create_program () in
  Gl.attach_shader program vshader ;
  Gl.attach_shader program fshader ;
  Gl.link_program program ;
  let link_status = get_int (Gl.get_programiv program Gl.link_status) in
  if link_status = Gl.false_ then
    fatal rc_OpenGL "Failed to link globe shader program" ;
  Gl.delete_shader vshader ;
  Gl.delete_shader fshader ;
  program

let setup_fullscreen_quad () =
  let vao = get_int (Gl.gen_vertex_arrays 1) in
  Gl.bind_vertex_array vao ;
  let vbo = get_int (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer vbo ;
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size quad_vertices)
    (Some quad_vertices) Gl.static_draw ;
  let stride = 4 * 4 in
  (* 4 floats per vertex: 2 pos + 2 uv, each float = 4 bytes *)
  Gl.vertex_attrib_pointer 0 2 Gl.float false stride (`Offset 0) ;
  Gl.enable_vertex_attrib_array 0 ;
  Gl.vertex_attrib_pointer 1 2 Gl.float false stride (`Offset (2 * 4)) ;
  Gl.enable_vertex_attrib_array 1 ;
  Gl.bind_vertex_array 0 ;
  vao

let sprogram = ref None

let fbo_atlastex = ref None

(* Called once per frame *)
let globe_render win frame_counter =
  if !need_to_flush_opengl_globe_cache then (
    fbo_atlastex := None ;
    need_to_flush_opengl_globe_cache := false
  ) ;
  let sprogram =
    match !sprogram with
    | None ->
        let x = compile_shaders () in
        sprogram := Some x ;
        x
    | Some x ->
        x
  in
  let quad_vao = setup_fullscreen_quad () in
  (* Render atlas to texture *)
  let fbo, atlas_texture =
    match !fbo_atlastex with
    | Some (f, a) ->
        (f, a)
    | None -> (
      match Atlas_screen_opengl.atlas_render ~to_texture:true win with
      | None ->
          fatal rc_OpenGL
            "Globe renderer didn't get FBO and atlas texture from atlas \
             renderer"
      | Some (f, a) ->
          fbo_atlastex := Some (f, a) ;
          (f, a) )
  in
  (* Bind default framebuffer to render globe to screen *)
  Gl.bind_framebuffer Gl.framebuffer 0 ;
  let win_w, win_h = Sdl.get_window_size win in
  Gl.viewport 0 0 win_w win_h ;
  Gl.clear_color 0.0 0.0 0.0 1.0 ;
  Gl.clear Gl.color_buffer_bit ;
  (* Use globe shader *)
  Gl.use_program sprogram ;
  Gl.bind_vertex_array quad_vao ;
  (* Set iResolution uniform *)
  let u_resolution = Gl.get_uniform_location sprogram "iResolution" in
  Gl.uniform2f u_resolution (float win_w) (float win_h) ;
  let u_time = Gl.get_uniform_location sprogram "iTime" in
  Gl.uniform1i u_time frame_counter ;
  let u_time = Gl.get_uniform_location sprogram "iLat" in
  Gl.uniform1i u_time !rotation_lat ;
  let u_time = Gl.get_uniform_location sprogram "iLon" in
  Gl.uniform1i u_time !rotation_lon ;
  (* Bind atlas texture to iChannel0 = texture unit 0 *)
  Gl.active_texture Gl.texture0 ;
  Gl.bind_texture Gl.texture_2d atlas_texture ;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest ;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest ;
  let u_ichannel0 = Gl.get_uniform_location sprogram "iChannel0" in
  Gl.uniform1i u_ichannel0 0 ;
  (* Draw *)
  Gl.draw_arrays Gl.triangles 0 6 ;
  (* Swap *)
  Sdl.gl_swap_window win
