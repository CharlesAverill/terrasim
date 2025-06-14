(** OpenGL program to draw the atlas screen *)

open Tsdl
open Tgl4
open Utils.Standard_utils
open Utils.Logging
open Utils.Opengl_utils
open Utils.Sdl_utils
open World.Grid
open World.Altitude
open World.Biomes
open Cameras.Atlas_camera
open Gradients
open Ui_texture

(** Triangle vertices that assemble to a quad to render onto *)
let quad_verts =
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    [| 0.; 0.; 1.; 0.; 1.; 1.; 0.; 0.; 1.; 1.; 0.; 1. |]

(** The
    {{:https://github.com/CharlesAverill/terrasim/blob/main/shaders/atlas.vert}
     atlas vertex shader} *)
let atlas_vertex_shader_src = [%blob "shaders/atlas.vert"]

(** The
    {{:https://github.com/CharlesAverill/terrasim/blob/main/shaders/atlas.frag}
     atlas fragment shader} *)
let atlas_fragment_shader_src = [%blob "shaders/atlas.frag"]

let ui_vertex_shader_src = [%blob "shaders/ui.vert"]
let ui_fragment_shader_src = [%blob "shaders/ui.frag"]

(** Set up tile buffers and return the VAO
    @param offsets Array of [(x, y)] offsets for each tile
    @param colors Array of [(r, g, b)] colors for each tile
    @return VAO bound to tile buffer *)
let setup_tile_buffers ~(offsets : (float * float) array)
    ~(colors : (float * float * float) array) : int =
  let vao = get_int (Gl.gen_vertex_arrays 1) in
  Gl.bind_vertex_array vao;
  (* Vertex buffer for quad *)
  let quad_vbo = get_int (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer quad_vbo;
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size quad_verts)
    (Some quad_verts) Gl.static_draw;
  Gl.vertex_attrib_pointer 0 2 Gl.float false 0 (`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  (* Interleave instance data: offset (2 floats) + color (3 floats) *)
  let num_tiles = Array.length offsets in
  let instance_data =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (num_tiles * 5)
  in
  Array.iteri
    (fun i (ox, oy) ->
      let r, g, b = colors.(i) in
      let i5 = i * 5 in
      instance_data.{i5 + 0} <- ox;
      instance_data.{i5 + 1} <- oy;
      instance_data.{i5 + 2} <- r;
      instance_data.{i5 + 3} <- g;
      instance_data.{i5 + 4} <- b)
    offsets;
  (* Instance buffer *)
  let instance_vbo = get_int (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer instance_vbo;
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size instance_data)
    (Some instance_data) Gl.static_draw;
  (* 5 floats per instance * 4 bytes per float *)
  let stride = 5 * 4 in
  (* Offset: location = 1 *)
  Gl.vertex_attrib_pointer 1 2 Gl.float false stride (`Offset 0);
  Gl.enable_vertex_attrib_array 1;
  Gl.vertex_attrib_divisor 1 1;
  (* Color: location = 2 *)
  Gl.vertex_attrib_pointer 2 3 Gl.float false stride (`Offset (2 * 4));
  Gl.enable_vertex_attrib_array 2;
  Gl.vertex_attrib_divisor 2 1;
  Gl.bind_vertex_array 0;
  vao

let draw_ui ui_sprogram =
  Gl.use_program ui_sprogram;
  Gl.enable Gl.blend;
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha;
  let quad_vao = setup_fullscreen_quad () in
  Gl.bind_vertex_array quad_vao;
  let gl_texture = get_int (Gl.gen_textures 1) in
  let w, h = Sdl.get_window_size (get_ui_window ()) in
  let texture_data = bigarray_create Bigarray.int8_unsigned (h * w * 4) in
  let* _ =
    Sdl.render_read_pixels (get_ui_renderer ()) None
      (Some Sdl.Pixel.format_rgba8888) texture_data (w * 4)
  in
  let texture_data = flip_texture_vertically texture_data w h in
  Gl.active_texture Gl.texture0;
  Gl.bind_texture Gl.texture_2d gl_texture;
  Gl.tex_image2d Gl.texture_2d 0 Gl.rgba w h 0 Gl.rgba Gl.unsigned_byte
    (`Data texture_data);
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  let tex_loc = Gl.get_uniform_location ui_sprogram "u_texture" in
  Gl.uniform1i tex_loc 0;
  Gl.draw_arrays Gl.triangles 0 6

(** Render tiles to the screen or a framebuffer
    @param fbo
      If set, render to this framebuffer rather than the screen (used by
      {!Globe.render_globe_screen})
    @param window Application's SDL window
    @param sprogram Shader program to use
    @param ui_sprogram Shader program to use for the UI
    @param vao Vertex array object to draw
    @param num_instances Number of tiles to draw *)
let render_tiles ?(fbo : Sdl.uint8 option = None) (window : Sdl.window)
    (sprogram : int) (ui_sprogram : int) (vao : int) (num_instances : int) =
  Gl.bind_framebuffer Gl.framebuffer
    (match fbo with Some fb -> fb | None -> 0);
  let w, h =
    if fbo = None then
      Sdl.get_window_size window
    else
      (512, 512)
  in
  Gl.viewport 0 0 w h;
  Gl.clear_color 0. 0. 0. 1.;
  Gl.clear Gl.color_buffer_bit;
  Gl.use_program sprogram;
  Gl.bind_vertex_array vao;
  let u_scale_x = Gl.get_uniform_location sprogram "uScaleX" in
  Gl.uniform1f u_scale_x (2. /. float world_width);
  let u_scale_y = Gl.get_uniform_location sprogram "uScaleY" in
  Gl.uniform1f u_scale_y (2. /. float world_height);
  Gl.draw_arrays_instanced Gl.triangles 0 6 num_instances;
  Gl.bind_framebuffer Gl.framebuffer 0;
  if fbo = None then (
    draw_ui ui_sprogram;
    Sdl.gl_swap_window window
  )

(** Create a framebuffer to render onto
    @param width Width of framebuffer
    @param height Height of framebuffer
    @return
      [(fbo, tex)] where [fbo] is the framebuffer and [tex] is the texture
      object *)
let create_render_texture ~(width : int) ~(height : int) : int * int =
  let tex = get_int (Gl.gen_textures 1) in
  Gl.bind_texture Gl.texture_2d tex;
  let texture_data =
    bigarray_create Bigarray.int8_unsigned (height * width * 4)
  in
  Gl.tex_image2d Gl.texture_2d 0 Gl.rgba width height 0 Gl.rgba Gl.unsigned_byte
    (`Data texture_data);
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.bind_texture Gl.texture_2d 0;
  let fbo = get_int (Gl.gen_framebuffers 1) in
  Gl.bind_framebuffer Gl.framebuffer fbo;
  Gl.framebuffer_texture2d Gl.framebuffer Gl.color_attachment0 Gl.texture_2d tex
    0;
  (* Check framebuffer completeness *)
  let status = Gl.check_framebuffer_status Gl.framebuffer in
  if status <> Gl.framebuffer_complete then
    fatal rc_OpenGL "Framebuffer not complete: %d" status;
  Gl.bind_framebuffer Gl.framebuffer 0;
  (fbo, tex)

(** Iterate over the world grid to populate offset and color arrays for each
    tile
    @return [(offsets, colors)] *)
let make_tile_data ?(use_atlas_camera : bool = true) () :
    (float * float) array * (float * float * float) array =
  let num_tiles = world_width * world_height in
  let offsets = Array.make num_tiles (0.0, 0.0) in
  let colors = Array.make num_tiles (0.0, 0.0, 0.0) in
  (* NDC units *)
  let scale_x = 2.0 /. float world_width in
  let scale_y = 2.0 /. float world_height in
  (* Render selected map mode *)
  Atlas_views.render_func () use_atlas_camera (scale_x, scale_y) offsets colors;
  (offsets, colors)

(** Shader program compiled for the current OpenGL context, or [None] *)
let sprogram = ref None

let ui_sprogram = ref None

(** Render the atlas screen
    @param to_texture Render to [window] if [false], otherwise to a new texture
    @param window Application's SDL window
    @return if [to_texture] then a texture else [None] *)
let render_atlas_screen ?(to_texture : bool = false) (window : Sdl.window) :
    int option =
  let sprogram, ui_sprogram =
    match (!sprogram, !ui_sprogram) with
    | Some x, Some y ->
        (x, y)
    | _, _ ->
        let x =
          compile_shaders atlas_vertex_shader_src atlas_fragment_shader_src
        in
        let y = compile_shaders ui_vertex_shader_src ui_fragment_shader_src in
        sprogram := Some x;
        ui_sprogram := Some y;
        (x, y)
  in
  let offsets, colors = make_tile_data ~use_atlas_camera:(not to_texture) () in
  let vao = setup_tile_buffers ~offsets ~colors in
  let fbo, tex =
    if to_texture then
      let x, y = create_render_texture ~width:512 ~height:512 in
      (Some x, y)
    else
      (None, 0)
  in
  render_tiles ~fbo window sprogram ui_sprogram vao (Array.length offsets);
  if fbo = None then
    None
  else
    Some tex
