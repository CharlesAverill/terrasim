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
  Gl.compile_shader vshader;
  (* Check if compiling shader errored *)
  if get_int (Gl.get_shaderiv vshader Gl.compile_status) = Gl.false_ then (
    let len = get_int (Gl.get_shaderiv vshader Gl.info_log_length) in
    let log = get_string len (Gl.get_shader_info_log vshader len None) in
    Gl.delete_shader vshader;
    fatal rc_OpenGL "Failed to compile vertex shader: %s" log
  );
  (* Compile the fragment shader *)
  let fshader = Gl.create_shader Gl.fragment_shader in
  Gl.shader_source fshader frag_src;
  Gl.compile_shader fshader;
  if get_int (Gl.get_shaderiv fshader Gl.compile_status) = Gl.false_ then (
    let len = get_int (Gl.get_shaderiv fshader Gl.info_log_length) in
    let log = get_string len (Gl.get_shader_info_log fshader len None) in
    Gl.delete_shader fshader;
    fatal rc_OpenGL "Failed to compile fragment shader: %s" log
  );
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
