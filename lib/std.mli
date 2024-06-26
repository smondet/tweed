open! Base

(** S for “Signal” *)
module S : sig
  type 'a t = 'a Lwd.t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  module O : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
    val return : 'a -> 'a t
  end
end

(** V for “Variable” *)
module V : sig
  type 'a t = 'a Lwd.var

  val make : 'a -> 'a t
  val get : 'a t -> 'a S.t
  val set : 'a t -> 'a -> unit
  val peek : 'a t -> 'a
  val toggle : bool t -> unit
end

(* module Extra_nottui = Extra_nottui *)

(** The main type of TUI-documents is {!Ui_doc.t} *)
module Ui_doc : sig
  type t = Nottui.ui Lwd.t
end

open Ui_doc

(* module Internal = Internal *)
module Attr : module type of Notty.A

val ( let* ) : 'a S.t -> ('a -> 'b S.t) -> 'b S.t
val ( and* ) : 'a S.t -> 'b S.t -> ('a * 'b) S.t
val return : 'a -> 'a S.t
val verbatim : ?attr:Attr.t -> string -> t
val hbox : t list -> t
val vbox : t list -> t
val zbox : t list -> t
val button : ?attr:Attr.t -> string -> action:(unit -> unit) -> t
val hspace : int -> t

val ( % ) : t -> t -> t
(** Concatenate horizontally. *)

val ( %% ) : t -> t -> t
(** Concatenate horizontally with a space in the middle. *)

val ( --- ) : t -> t -> t
(** Concatenate vertically. *)

val keyboard_area :
  ?focus:Nottui.Focus.status ->
  t ->
  handler:(Nottui.Ui.key -> Nottui.Ui.may_handle) ->
  t

val scrollbox : t -> t
val scroll_vbox : t list -> t
val text : ?attr:Attr.t -> string -> t
val bold : string -> t
val empty : unit -> t

val grid :
  ?max_h:int ->
  ?max_w:int ->
  ?pad:Nottui.gravity ->
  ?crop:Nottui.gravity ->
  ?bg:Notty.attr ->
  ?h_space:int ->
  ?v_space:int ->
  ?headers:t list ->
  t list list ->
  t

val text_input :
  ?focus:Nottui.Focus.handle ->
  (string * int) Lwd.t ->
  on_change:(string * int -> unit) ->
  on_submit:(string * int -> unit) ->
  t

val space : int -> int -> t
val local_timestamp : float -> string

module Debug : sig
  val if_var : bool Lwd.var -> (unit -> t) -> t
  val verbatim : string -> t
  val sexpable : ('a -> Sexp.t) -> 'a -> t
end

module Errors : sig
  module Error : sig
    type t = {
      date : float; [@default Unix.gettimeofday ()]
      blob : string; [@main]
      section : string; [@default ""]
    }
    [@@deriving fields, make]
  end

  type t = { stack : Error.t list V.t [@default V.make []] }
  [@@deriving fields, make]

  val add : t -> ?date:float -> ?section:string -> string -> unit
  val add_exn : t -> ?date:float -> ?section:string -> exn -> unit
  val protect : ?section:string -> t -> (unit -> unit) -> unit
  val clear : t -> unit

  val render_opt :
    ?collapse_similar:bool -> ?with_date:bool -> t -> Ui_doc.t option Lwd.t

  val render : ?with_date:bool -> ?collapse_similar:bool -> t -> Ui_doc.t
end

module Modal_shortcuts : sig
  module Mode : sig
    type 'tag action_result =
      [ `Home | `Mode of 'tag t | `Prompt of 'tag prompt | `Quit | `Stay | `Up ]

    and 'tag prompt = {
      label : string Lwd.t;
      action : string -> 'tag action_result;
      initial_value : string;
    }

    and 'tag shortcut =
      | On_key of {
          key : char option;
          name : string Lwd.t;
          action : unit -> 'tag action_result;
        }

    and 'tag t = {
      tag : 'tag option;
      vertical : bool Lwd.t;
      bindings : 'tag shortcut list Lwd.t;
    }

    val entry :
      ?key:char ->
      string Lwd.t ->
      action:(unit -> 'a action_result) ->
      'a shortcut

    val on_key :
      char -> string -> action:(unit -> 'a action_result) -> 'a shortcut

    val on_key_dyn :
      char -> string Lwd.t -> action:(unit -> 'a action_result) -> 'a shortcut

    val just_match : string -> action:(unit -> 'a action_result) -> 'a shortcut

    val prompt :
      label:string Lwd.t ->
      ?initial_value:string ->
      (string -> 'a action_result) ->
      'a prompt

    val mode_dyn :
      ?vertical:bool Lwd.t -> ?tag:'a -> 'a shortcut list Lwd.t -> 'a t

    val mode : ?vertical:bool Lwd.t -> ?tag:'a -> 'a shortcut list -> 'a t
    val tag : 'a t -> 'a option
  end

  module Modifier : sig
    type t = [ `Ctrl | `Meta | `Shift ]
    [@@deriving sexp, compare, equal, variants, show]
  end

  type 'tag t = {
    root : 'tag Mode.t; [@main]
    mode_stack : 'tag Mode.t list V.t; [@default V.make []]
    input_mode : [ `Single_key | `Match | `Prompting of 'tag Mode.prompt ] V.t;
        [@default V.make `Single_key]
    match_text : string V.t; [@default V.make ""]
    errors : Errors.t; [@default Errors.make ()]
    choice : int V.t; [@default V.make 0]
    debug : bool V.t; [@default V.make false]
    modifier : Modifier.t option;
    navigation_bindings : [ `Vimish of Modifier.t ]; [@default `Vimish `Meta]
  }
  [@@deriving fields, make]

  val current_mode : 'a t -> 'a Mode.t Lwd.t
  val peek_current_mode : 'a t -> 'a Mode.t
  val filter_bindings : 'a t -> 'a Mode.shortcut list Lwd.t
  val render : ?scoped_ui:(Ui_doc.t -> Ui_doc.t) -> 'a t -> Ui_doc.t
end

module Run_ui : sig
  val start :
    ?tick_period:float ->
    ?tick:(unit -> unit) ->
    ?term:Notty_unix.Term.t ->
    ?renderer:Nottui.Renderer.t ->
    ?quit:bool Lwd.var ->
    ?quit_on_escape:bool ->
    ?quit_on_ctrl_q:bool ->
    Ui_doc.t ->
    unit
end

module Nottui_widgets : sig end
module Nottui : sig end
module Notty : sig end
