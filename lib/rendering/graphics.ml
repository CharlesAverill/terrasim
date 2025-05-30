(** Handles graphics initialization and swapping between SDL and OpenGL *)

open Utils.Logging
open Utils.Sdl_utils
open Utils.Globals
open Tsdl
open Assets.Assetloader
open Utils
open Globe_data

(** Initialize SDL and SDL-ttf *)
let init_sdl () =
  let* _ = Sdl.init Sdl.Init.(video + events) in
  let _ =
    Tsdl_image.Image.init
      (let open Tsdl_image.Image.Init in
       png)
  in
  let* _ = Tsdl_ttf.Ttf.init () in
  ()

(** Create an SDL window
    @param w Width
    @param h Height
    @param window_name Title of window
    @return SDL window object for application *)
let create_window ?(w : int = 1920) ?(h : int = 1080) (window_name : string) :
    Sdl.window =
  let* w =
    Sdl.create_window ~w ~h window_name
      (let open Sdl.Window in
       opengl + fullscreen_desktop)
  in
  Sdl.set_window_minimum_size w ~w:1280 ~h:720;
  w

(** @param window The application's SDL window
    @return New SDL renderer object for [window] *)
let create_renderer (window : Sdl.window) : Sdl.renderer =
  match
    Sdl.create_renderer
      ~flags:
        (let open Sdl.Renderer in
         accelerated + presentvsync)
      window
  with
  | Error _ ->
      let* r =
        Sdl.create_renderer
          ~flags:
            (let open Sdl.Renderer in
             software + presentvsync)
          window
      in
      r
  | Ok r ->
      r

(** @param window Application's SDL window
    @return OpenGL context for [window] *)
let get_opengl_context (window : Sdl.window) : Sdl.gl_context =
  let* ctx = Sdl.gl_create_context window in
  ctx

(** @param window Application's SDL window
    @param blob Image blob *)
let set_window_icon (window : Sdl.window) blob =
  Sdl.set_window_icon window (surface_of_blob blob)

(** Status of current rendering setup *)
type render_mode =
  | UninitRender  (** Uninitialized rendering *)
  | SdlRender of Sdl.renderer  (** SDL rendering *)
  | GlRender of Sdl.gl_context  (** OpenGL rendering *)

(** Current rendering setup status *)
let current_render_mode = ref UninitRender

(** Start up SDL rendering
    @param gl_ctx Optional OpenGL context to delete
    @param window Application's SDL window
    @return SDL renderer *)
let use_sdl ?(gl_ctx = None) (window : Sdl.window) : Sdl.renderer =
  (match gl_ctx with None -> () | Some x -> Sdl.gl_delete_context x);
  create_renderer window

(** Start up OpenGL rendering
    @param window Application's SDL window
    @param renderer SDL renderer to destroy
    @return OpenGL context *)
let use_opengl (window : Sdl.window) (renderer : Sdl.renderer) : Sdl.gl_context
    =
  Sdl.destroy_renderer renderer;
  get_opengl_context window

(** Swap between SDL <-> OpenGL rendering
    @param window Application's SDL window *)
let swap_render_mode (window : Sdl.window) =
  (* Edit screen reset *)
  clear_edit_cache ();
  (* Atlas screen reset *)
  Atlas.sprogram := None;
  (* Globe screen reset *)
  Globe.sprogram := None;
  Globe.atlastex := None;
  current_render_mode :=
    match !current_render_mode with
    | UninitRender ->
        SdlRender (use_sdl window)
    | SdlRender r ->
        GlRender (use_opengl window r)
    | GlRender g ->
        SdlRender (use_sdl ~gl_ctx:(Some g) window)

(** Get the current global SDL renderer, or error if not in SDL render mode
    @return Current SDL renderer *)
let get_global_renderer () : Sdl.renderer =
  match !current_render_mode with
  | SdlRender r ->
      r
  | _ ->
      fatal rc_Error "Tried to get SDL renderer when not in SDL mode"

(** Get the current global OpenGL context, or error if not in OpenGL render mode
    @return Current OpenGL context *)
let get_global_gl_ctx () : Sdl.gl_context =
  match !current_render_mode with
  | GlRender g ->
      g
  | _ ->
      fatal rc_Error "Tried to get GL context when not in GL mode"
