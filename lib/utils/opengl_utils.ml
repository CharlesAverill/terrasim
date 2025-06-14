(** Utilities for OpenGL *)

open Tgl4
open Logging

(** Create a 1D [Bigarray] of kind [k] and length [len] with C layout
    @param k
    @param len *)
let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

(** Get an integer result from OpenGL
    @param f Operation to get result from
    @return Integer result of running [f] *)
let get_int f : int =
  let a = bigarray_create Bigarray.int32 1 in
  f a;
  Int32.to_int a.{0}

let get_intv size f =
  let a = bigarray_create Bigarray.int32 size in
  f a;
  a

(** Get a string result from OpenGL
    @param len Length of string to read
    @param f Operation to get result from
    @return String result of running [f] *)
let get_string (len : int) f : string =
  let a = bigarray_create Bigarray.char len in
  f a;
  Gl.string_of_bigarray a

(** Compile vertex and fragment shaders into a shader program
    @param vert_src Vertex shader source code
    @param frag_src Fragment shader source code
    @return Shader program *)
let compile_shaders (vert_src : string) (frag_src : string) : int =
  (* Compile the vertex shader *)
  let vshader = Gl.create_shader Gl.vertex_shader in
  Gl.shader_source vshader vert_src;
  let shader_status = ref Gl.false_ in
  while !shader_status = Gl.false_ do
    Gl.compile_shader vshader;
    shader_status := get_int (Gl.get_shaderiv vshader Gl.compile_status);
    (* Check if compiling shader errored *)
    if !shader_status = Gl.false_ then (
      let len = get_int (Gl.get_shaderiv vshader Gl.info_log_length) in
      let log = get_string len (Gl.get_shader_info_log vshader len None) in
      Gl.delete_shader vshader;
      _log Log_Error "Failed to compile vertex shader: %s" log
    )
  done;
  (* Compile the fragment shader *)
  let fshader = Gl.create_shader Gl.fragment_shader in
  Gl.shader_source fshader frag_src;
  let shader_status = ref Gl.false_ in
  while !shader_status = Gl.false_ do
    Gl.compile_shader fshader;
    shader_status := get_int (Gl.get_shaderiv fshader Gl.compile_status);
    (* Check if compiling shader errored *)
    if !shader_status = Gl.false_ then (
      let len = get_int (Gl.get_shaderiv fshader Gl.info_log_length) in
      let log = get_string len (Gl.get_shader_info_log fshader len None) in
      Gl.delete_shader fshader;
      _log Log_Error "Failed to compile fragment shader: %s" log
    )
  done;
  (* Combine vertex + fragment shaders into "shader program" and use it *)
  let sprogram = Gl.create_program () in
  Gl.attach_shader sprogram vshader;
  Gl.attach_shader sprogram fshader;
  Gl.link_program sprogram;
  (* Check if linking shader program errored *)
  if get_int (Gl.get_programiv sprogram Gl.link_status) = Gl.false_ then
    fatal rc_OpenGL "Failed to link shader program";
  (* Delete shader objects now that they're on the GPU *)
  Gl.delete_shader vshader;
  Gl.delete_shader fshader;
  sprogram

(** Triangle vertices that assemble to a quad to render onto, packed with UVs *)
let quad_vertices =
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    [|
      -1.0;
      -1.0;
      0.0;
      0.0;
      1.0;
      -1.0;
      1.0;
      0.0;
      1.0;
      1.0;
      1.0;
      1.0;
      -1.0;
      -1.0;
      0.0;
      0.0;
      1.0;
      1.0;
      1.0;
      1.0;
      -1.0;
      1.0;
      0.0;
      1.0;
    |]

(** Set up fullscreen quad buffer and return the VAO
    @return VAO bound to quad buffer *)
let setup_fullscreen_quad () =
  let vao = get_int (Gl.gen_vertex_arrays 1) in
  Gl.bind_vertex_array vao;
  let vbo = get_int (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer vbo;
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size quad_vertices)
    (Some quad_vertices) Gl.static_draw;
  let stride = 4 * 4 in
  (* 4 floats per vertex: 2 pos + 2 uv, each float = 4 bytes *)
  Gl.vertex_attrib_pointer 0 2 Gl.float false stride (`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  Gl.vertex_attrib_pointer 1 2 Gl.float false stride (`Offset (2 * 4));
  Gl.enable_vertex_attrib_array 1;
  Gl.bind_vertex_array 0;
  vao

let flip_texture_vertically
    (data :
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t)
    (width : int) (height : int) :
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
  let row_size = width * 4 in
  (* RGBA = 4 bytes per pixel *)
  let flipped =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (row_size * height)
  in
  for row = 0 to height - 1 do
    let src_off = row * row_size in
    let dst_off = (height - 1 - row) * row_size in
    for i = 0 to row_size - 1 do
      Bigarray.Array1.set flipped (dst_off + i)
        (Bigarray.Array1.get data (src_off + i))
    done
  done;
  flipped
