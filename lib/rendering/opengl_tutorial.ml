open Tsdl
open Sdl
open Tgl4
open Logging
open Opengl_utils

let tri_verts = verts_of_list [(-0.8, -0.8, 0.); (0.8, -0.8, 0.); (0., 0.8, 0.)]

let basic_vertex_shader_src = [%blob "shaders/basic_shader.vert"]

let basic_fragment_shader_src = [%blob "shaders/basic_shader.frag"]

let compile_shaders () =
  (* Compile the vertex shader *)
  let vshader = Gl.create_shader Gl.vertex_shader in
  Gl.shader_source vshader basic_vertex_shader_src ;
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
  Gl.shader_source fshader basic_fragment_shader_src ;
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

let put_tri_on_gpu () =
  (* Create vertex array object *)
  let vao = get_int (Gl.gen_vertex_arrays 1) in
  Gl.bind_vertex_array vao ;
  (* Create vertex buffer array *)
  let vbo = get_int (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer vbo ;
  (* Copy vertex data into vertex buffer *)
  (* static_draw is for data that doesn't change frequently and is used often *)
  (* dynamic_draw necessary for frequently-changing data *)
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size tri_verts)
    (Some tri_verts) Gl.static_draw ;
  (* Attribute vertex data to aPos in basic_shader.vert *)
  Gl.vertex_attrib_pointer 0 3 Gl.float false 0 (`Offset 0) ;
  Gl.enable_vertex_attrib_array 0 ;
  (* Unbind vao to prevent interference *)
  Gl.bind_vertex_array 0 ;
  vao

let render_body win sprogram vao =
  (* Clear screen *)
  Gl.clear_color 0.2 0.3 0.3 1. ;
  Gl.clear Gl.color_buffer_bit ;
  (* Use the shader program *)
  Gl.use_program sprogram ;
  (* Bind the triangle data *)
  Gl.bind_vertex_array vao ;
  (* Draw the triangle *)
  Gl.draw_arrays Gl.triangles 0 3 ;
  (* Swap buffers *)
  Sdl.gl_swap_window win

let main window =
  let e = Sdl.Event.create () in
  let event e = Sdl.Event.(enum (get e typ)) in
  let window_event e = Sdl.Event.(window_event_enum (get e window_event_id)) in
  let loop_continue = ref true in
  let sprogram = compile_shaders () in
  let vao = put_tri_on_gpu () in
  while !loop_continue do
    while Sdl.poll_event (Some e) do
      if Event.get e Event.typ = Event.key_down then
        if Event.get e Event.keyboard_keycode = Sdl.K.escape then
          loop_continue := false ;
      render_body window sprogram vao
    done
  done
