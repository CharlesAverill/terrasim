open Tsdl
open Sdl
open Tgl4
open Logging
open Opengl_utils
open Worldgrid
open Altitude
open Utils
open Gradients
open Globals

let quad_verts =
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    [|0.; 0.; 1.; 0.; 1.; 1.; 0.; 0.; 1.; 1.; 0.; 1.|]

let globe_vertex_shader_src = [%blob "shaders/globe.vert"]

let globe_fragment_shader_src = [%blob "shaders/globe.frag"]

let compile_shaders () =
  (* Compile the vertex shader *)
  let vshader = Gl.create_shader Gl.vertex_shader in
  Gl.shader_source vshader globe_vertex_shader_src ;
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
  Gl.shader_source fshader globe_fragment_shader_src ;
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

let setup_globe_tile_buffers ~directions ~colors =
  let vao = get_int (Gl.gen_vertex_arrays 1) in
  Gl.bind_vertex_array vao ;
  (* Reuse quad VBO (same as globe renderer) *)
  let quad_vbo = get_int (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer quad_vbo ;
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size quad_verts)
    (Some quad_verts) Gl.static_draw ;
  Gl.vertex_attrib_pointer 0 2 Gl.float false 0 (`Offset 0) ;
  Gl.enable_vertex_attrib_array 0 ;
  (* Instance buffer: direction + color = 6 floats *)
  let num_tiles = Array.length directions in
  let instance_data =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (num_tiles * 6)
  in
  Array.iteri
    (fun i (dx, dy, dz) ->
      let r, g, b = colors.(i) in
      let i6 = i * 6 in
      instance_data.{i6 + 0} <- dx ;
      instance_data.{i6 + 1} <- dy ;
      instance_data.{i6 + 2} <- dz ;
      instance_data.{i6 + 3} <- r ;
      instance_data.{i6 + 4} <- g ;
      instance_data.{i6 + 5} <- b )
    directions ;
  let instance_vbo = get_int (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer instance_vbo ;
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size instance_data)
    (Some instance_data) Gl.static_draw ;
  let stride = 6 * 4 in
  Gl.vertex_attrib_pointer 1 3 Gl.float false stride (`Offset 0) ;
  Gl.enable_vertex_attrib_array 1 ;
  Gl.vertex_attrib_divisor 1 1 ;
  Gl.vertex_attrib_pointer 2 3 Gl.float false stride (`Offset (3 * 4)) ;
  Gl.enable_vertex_attrib_array 2 ;
  Gl.vertex_attrib_divisor 2 1 ;
  Gl.bind_vertex_array 0 ;
  vao

let generate_directions_and_colors () =
  let num_tiles = world_width * world_height in
  let directions = Array.make num_tiles (0., 0., 0.) in
  let colors = Array.make num_tiles (0., 0., 0.) in
  let altitudes = grid.altitude in
  let biomes = grid.biome in
  let idx = ref 0 in
  for j = 0 to world_height - 1 do
    for i = 0 to world_width - 1 do
      let lon = (float i /. float world_width *. 2. *. Float.pi) -. Float.pi in
      let lat =
        (float j /. float world_height *. Float.pi) -. (Float.pi /. 2.)
      in
      let x = cos lat *. sin lon in
      let y = -.sin lat in
      let z = cos lat *. cos lon in
      directions.(!idx) <- (x, y, z) ;
      let alt = altitudes.(!idx) in
      let biome = biomes.(!idx) in
      let norm_alt = clamp (float alt /. float max_land_height) 0.0 1.0 in
      let r, g, b =
        match ocean_height biome with
        | None ->
            interpolate_gradient height_gradient norm_alt
        | Some h ->
            interpolate_gradient ocean_gradient (clamp (float h /. 3.) 0. 1.)
      in
      colors.(!idx) <- (float r /. 255., float g /. 255., float b /. 255.) ;
      idx := !idx + 1
    done
  done ;
  (directions, colors)

let do_globe_render win sprogram vao num_instances =
  Gl.clear_color 0.0 0.0 0.0 1.0 ;
  Gl.clear Gl.color_buffer_bit ;
  Gl.use_program sprogram ;
  Gl.bind_vertex_array vao ;
  let uRotLon = Gl.get_uniform_location sprogram "uRotLon" in
  let uRotLat = Gl.get_uniform_location sprogram "uRotLat" in
  let uRadius = Gl.get_uniform_location sprogram "uRadius" in
  Gl.uniform1f uRotLon (float_of_int !rotation_lon *. Float.pi /. 180.) ;
  Gl.uniform1f uRotLat (float_of_int !rotation_lat *. Float.pi /. 180.) ;
  Gl.uniform1f uRadius 0.9 ;
  (* NDC radius *)
  Gl.draw_arrays_instanced Gl.triangles 0 6 num_instances ;
  Sdl.gl_swap_window win

let sprogram = ref None

let globe_render win =
  let sprogram =
    match !sprogram with
    | None ->
        let x = compile_shaders () in
        sprogram := Some x ;
        x
    | Some x ->
        x
  in
  let directions, colors = generate_directions_and_colors () in
  let vao = setup_globe_tile_buffers ~directions ~colors in
  do_globe_render win sprogram vao (Array.length directions)
