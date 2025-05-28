open Tsdl
open Sdl
open Tgl4
open Logging
open Opengl_utils
open Worldgrid
open Altitude
open Utils
open Atlas_camera
open Gradients

let quad_verts =
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    [|0.; 0.; 1.; 0.; 1.; 1.; 0.; 0.; 1.; 1.; 0.; 1.|]

let atlas_vertex_shader_src = [%blob "shaders/atlas.vert"]

let atlas_fragment_shader_src = [%blob "shaders/atlas.frag"]

let compile_shaders () =
  (* Compile the vertex shader *)
  let vshader = Gl.create_shader Gl.vertex_shader in
  Gl.shader_source vshader atlas_vertex_shader_src ;
  Gl.compile_shader vshader ;
  (* Check if compiling shader errored *)
  if get_int (Gl.get_shaderiv vshader Gl.compile_status) = Gl.false_ then (
    let len = get_int (Gl.get_shaderiv vshader Gl.info_log_length) in
    let log = get_string len (Gl.get_shader_info_log vshader len None) in
    Gl.delete_shader vshader ;
    fatal rc_OpenGL "Failed to compile vertex shader: %s" log
  ) ;
  (* Compile the fragment shader *)
  let fshader = Gl.create_shader Gl.fragment_shader in
  Gl.shader_source fshader atlas_fragment_shader_src ;
  Gl.compile_shader fshader ;
  if get_int (Gl.get_shaderiv fshader Gl.compile_status) = Gl.false_ then (
    let len = get_int (Gl.get_shaderiv fshader Gl.info_log_length) in
    let log = get_string len (Gl.get_shader_info_log fshader len None) in
    Gl.delete_shader fshader ;
    fatal rc_OpenGL "Failed to compile fragment shader: %s" log
  ) ;
  (* Combine vertex + fragment shaders into "shader program" and use it *)
  let sprogram = Gl.create_program () in
  Gl.attach_shader sprogram vshader ;
  Gl.attach_shader sprogram fshader ;
  Gl.link_program sprogram ;
  (* Check if linking shader program errored *)
  if get_int (Gl.get_programiv sprogram Gl.link_status) = Gl.false_ then
    fatal rc_OpenGL "Failed to link shader program" ;
  (* Delete shader objects now that they're on the GPU *)
  Gl.delete_shader vshader ;
  Gl.delete_shader fshader ;
  sprogram

let setup_tile_buffers ~offsets ~colors =
  let vao = get_int (Gl.gen_vertex_arrays 1) in
  Gl.bind_vertex_array vao ;
  (* Vertex buffer for quad *)
  let quad_vbo = get_int (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer quad_vbo ;
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size quad_verts)
    (Some quad_verts) Gl.static_draw ;
  Gl.vertex_attrib_pointer 0 2 Gl.float false 0 (`Offset 0) ;
  Gl.enable_vertex_attrib_array 0 ;
  (* Interleave instance data: offset (2 floats) + color (3 floats) *)
  let num_tiles = Array.length offsets in
  let instance_data =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (num_tiles * 5)
  in
  Array.iteri
    (fun i (ox, oy) ->
      let r, g, b = colors.(i) in
      let i5 = i * 5 in
      instance_data.{i5 + 0} <- ox ;
      instance_data.{i5 + 1} <- oy ;
      instance_data.{i5 + 2} <- r ;
      instance_data.{i5 + 3} <- g ;
      instance_data.{i5 + 4} <- b )
    offsets ;
  (* Instance buffer *)
  let instance_vbo = get_int (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer instance_vbo ;
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size instance_data)
    (Some instance_data) Gl.static_draw ;
  (* 5 floats per instance * 4 bytes per float *)
  let stride = 5 * 4 in
  (* Offset: location = 1 *)
  Gl.vertex_attrib_pointer 1 2 Gl.float false stride (`Offset 0) ;
  Gl.enable_vertex_attrib_array 1 ;
  Gl.vertex_attrib_divisor 1 1 ;
  (* Color: location = 2 *)
  Gl.vertex_attrib_pointer 2 3 Gl.float false stride (`Offset (2 * 4)) ;
  Gl.enable_vertex_attrib_array 2 ;
  Gl.vertex_attrib_divisor 2 1 ;
  Gl.bind_vertex_array 0 ;
  vao

let render_tiles ?(fbo : uint8 option = None) win sprogram vao num_instances =
  Gl.bind_framebuffer Gl.framebuffer (match fbo with Some fb -> fb | None -> 0) ;
  Gl.viewport 0 0
    ( if fbo = None then
        fst (Sdl.get_window_size win)
      else
        512 )
    (* or desired tex size *)
    ( if fbo = None then
        snd (Sdl.get_window_size win)
      else
        512 ) ;
  Gl.clear_color 0.1 0.1 0.1 1. ;
  Gl.clear Gl.color_buffer_bit ;
  Gl.use_program sprogram ;
  Gl.bind_vertex_array vao ;
  let u_scale = Gl.get_uniform_location sprogram "uScale" in
  Gl.uniform1f u_scale (1. /. 30.) ;
  Gl.draw_arrays_instanced Gl.triangles 0 6 num_instances ;
  Gl.bind_framebuffer Gl.framebuffer 0 ;
  if fbo = None then Sdl.gl_swap_window win

let create_render_texture ~width ~height =
  let tex = get_int (Gl.gen_textures 1) in
  Gl.bind_texture Gl.texture_2d tex ;
  let texture_data =
    let data = bigarray_create Bigarray.int8_unsigned (height * width * 4) in
    for i = 0 to (height * width) - 1 do
      set_3d data i 255 255 0
    done ;
    data
  in
  Gl.tex_image2d Gl.texture_2d 0 Gl.rgba width height 0 Gl.rgba Gl.unsigned_byte
    (`Data texture_data) ;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear ;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear ;
  Gl.bind_texture Gl.texture_2d 0 ;
  let fbo = get_int (Gl.gen_framebuffers 1) in
  Gl.bind_framebuffer Gl.framebuffer fbo ;
  Gl.framebuffer_texture2d Gl.framebuffer Gl.color_attachment0 Gl.texture_2d tex
    0 ;
  (* Optionally check framebuffer completeness *)
  let status = Gl.check_framebuffer_status Gl.framebuffer in
  if status <> Gl.framebuffer_complete then
    fatal rc_OpenGL "Framebuffer not complete: %d" status ;
  Gl.bind_framebuffer Gl.framebuffer 0 ;
  (fbo, tex)

let make_tile_data () =
  let num_tiles = world_width * world_height in
  let offsets = Array.make num_tiles (0.0, 0.0) in
  let colors = Array.make num_tiles (0.0, 0.0, 0.0) in
  (* NDC units *)
  let scale_x = 2.0 /. float world_width in
  let scale_y = 2.0 /. float world_height in
  let altitudes = grid.altitude in
  let biomes = grid.biome in
  let idx = ref 0 in
  Array.iter2
    (fun alt biome ->
      let norm_alt = clamp (float alt /. float max_land_height) 0.0 1.0 in
      let r, g, b =
        match ocean_height biome with
        | None ->
            interpolate_gradient height_gradient norm_alt
        | Some h ->
            interpolate_gradient ocean_gradient (clamp (float h /. 3.) 0. 1.)
      in
      let wx, wy = (!idx mod world_width, !idx / world_width) in
      let wx = mod_wrap (wx + atlas_camera.x) world_width in
      (* Convert to NDC position of bottom-left corner of tile *)
      let ndc_x = -1.0 +. (float wx *. scale_x) in
      let ndc_y = -1.0 +. (float wy *. scale_y) in
      offsets.(!idx) <- (ndc_x, -.ndc_y -. scale_y) ;
      colors.(!idx) <- (float r /. 255., float g /. 255., float b /. 255.) ;
      idx := !idx + 1 )
    altitudes biomes ;
  (offsets, colors)

let sprogram = ref None

let atlas_render ?(to_texture = false) win =
  let sprogram =
    match !sprogram with
    | None ->
        let x = compile_shaders () in
        sprogram := Some x ;
        x
    | Some x ->
        x
  in
  let offsets, colors = make_tile_data () in
  let vao = setup_tile_buffers ~offsets ~colors in
  let fbo, tex =
    if to_texture then
      let x, y = create_render_texture ~width:512 ~height:512 in
      (Some x, y)
    else
      (None, 0)
  in
  render_tiles ~fbo win sprogram vao (Array.length offsets) ;
  if fbo = None then
    None
  else
    Some (fbo, tex)
