open! Base

module V = struct
  type 'a t = 'a Lwd.var

  let make v = Lwd.var v
  let get = Lwd.get
  let set = Lwd.set
  let peek = Lwd.peek
  let toggle v = set v (not (peek v))
end

module S = struct
  type 'a t = 'a Lwd.t

  let map = Lwd.map
  let bind = Lwd.bind

  module O = struct
    let ( let* ) x f = Lwd.bind x ~f
    let ( and* ) = Lwd.pair
    let return = Lwd.pure
  end
end

module Extra_nottui = struct
  open! Lwd.Infix
  open! Notty
  open! Nottui

  let mini, maxi, clampi = Lwd_utils.(mini, maxi, clampi)
  let attr_clickable = A.(bg lightblue)

  let sub' str p l =
    if p = 0 && l = String.length str then str else String.sub str ~pos:p ~len:l

  let edit_field ?(focus = Focus.make ()) state ~on_change ~on_submit =
    let update focus_h focus (text, pos) =
      let pos = clampi pos ~min:0 ~max:(String.length text) in
      let content =
        Ui.atom @@ I.hcat
        @@
        if Focus.has_focus focus then
          let attr = attr_clickable in
          let len = String.length text in
          (if pos >= len then [ I.string attr text ]
          else [ I.string attr (sub' text 0 pos) ])
          @
          if pos < String.length text then
            [
              I.string A.(bg lightred) (sub' text pos 1);
              I.string attr (sub' text (pos + 1) (len - pos - 1));
            ]
          else [ I.string A.(bg lightred) " " ]
        else
          [ I.string A.(st underline) String.(if text = "" then " " else text) ]
      in
      let handler = function
        | `ASCII 'U', [ `Ctrl ] ->
            on_change ("", 0);
            `Handled (* clear *)
        | `Escape, [] ->
            Focus.release focus_h;
            `Handled
        | `ASCII k, [] ->
            let text =
              if pos < String.length text then
                Stdlib.String.sub text 0 pos
                ^ String.make 1 k
                ^ Stdlib.String.sub text pos (String.length text - pos)
              else text ^ String.make 1 k
            in
            on_change (text, pos + 1);
            `Handled
        | `Backspace, [] ->
            let text =
              if pos > 0 then
                if pos < String.length text then
                  Stdlib.String.sub text 0 (pos - 1)
                  ^ Stdlib.String.sub text pos (String.length text - pos)
                else if String.length text > 0 then
                  Stdlib.String.sub text 0 (String.length text - 1)
                else text
              else text
            in
            let pos = maxi 0 (pos - 1) in
            on_change (text, pos);
            `Handled
        | `Enter, [] ->
            on_submit (text, pos);
            `Handled
        | `Arrow `Left, [] ->
            let pos = mini (String.length text) pos in
            if pos > 0 then (
              on_change (text, pos - 1);
              `Handled)
            else `Unhandled
        | `Arrow `Right, [] ->
            let pos = pos + 1 in
            if pos <= String.length text then (
              on_change (text, pos);
              `Handled)
            else `Unhandled
        | _ -> `Unhandled
      in
      Ui.keyboard_area ~focus handler content
    in
    let node = Lwd.map2 ~f:(update focus) (Focus.status focus) state in
    let mouse_grab (text, pos) ~x ~y:_ = function
      | `Left ->
          if x <> pos then on_change (text, x);
          Nottui.Focus.request focus;
          `Handled
      | _ -> `Unhandled
    in
    Lwd.map2 state node ~f:(fun state content ->
        Ui.mouse_area (mouse_grab state) content)
end

type t = Nottui.ui Lwd.t

module Internal = struct
  let pack : 'a Lwd_utils.monoid -> t list -> t = Lwd_utils.pack
  let of_ui x : t = Lwd.pure x
end

open Nottui
open Internal
module Attr = Notty.A
include S.O

let verbatim ?attr s : t = Lwd.pure (Nottui_widgets.string ?attr s)
let hbox : t list -> t = fun l -> pack Ui.pack_x l
let vbox l = pack Ui.pack_y l
let zbox l = pack Ui.pack_z l
let button ?attr s ~action = of_ui (Nottui_widgets.button ?attr s action)
let ( % ) a b = hbox [ a; b ]
let hspace n = Ui.space n 0 |> of_ui
let ( %% ) a b = hbox [ a; hspace 1; b ]
let ( --- ) a b = vbox [ a; b ]

let keyboard_area ?focus content ~handler : t =
  Lwd.map content ~f:(fun ui -> Ui.keyboard_area ?focus handler ui)

let scrollbox : t -> t = Nottui_widgets.scrollbox
let scroll_vbox l = scrollbox (vbox l)
let text ?attr s : t = Fmt.kstr (verbatim ?attr) "@[%a@]" Fmt.text s
let bold s : t = text ~attr:Attr.(st bold) s
let empty () = Nottui_widgets.empty_lwd
let grid = Nottui_widgets.grid
let text_input = Extra_nottui.edit_field
let space x y = return (Ui.space x y)

let local_timestamp f =
  let open Unix in
  let gm = localtime f in
  Fmt.str "%04d-%02d-%02d-%02d:%02d:%02d" (1900 + gm.tm_year) (gm.tm_mon + 1)
    gm.tm_mday gm.tm_hour gm.tm_min gm.tm_sec

module Debug = struct
  let if_var v f =
    V.get v |> S.bind ~f:(function true -> f () | false -> empty ())

  let verbatim = verbatim ~attr:Attr.(fg yellow)
  let sexpable to_sexp x = Fmt.kstr verbatim "%a" Sexp.pp_hum (to_sexp x)
end

module Errors = struct
  module Error = struct
    type t = {
      date : float; [@default Unix.gettimeofday ()]
      blob : string; [@main]
      section : string; [@default ""]
    }
    [@@deriving fields, make]
  end

  type t = { stack : Error.t list V.t [@default V.make []] }
  [@@deriving fields, make]

  let add self ?date ?section s =
    V.set self.stack (Error.make ?date ?section s :: V.peek self.stack)

  let add_exn self ?date ?section e =
    Fmt.kstr (add self ?date ?section) "%a" Exn.pp e

  let protect ?section self f = try f () with e -> add_exn self ?section e
  let clear self = V.set self.stack []

  let render_opt ?(collapse_similar = false) ?(with_date = false) self =
    S.map (V.get self.stack) ~f:(function
      | [] -> None
      | more ->
          Some
            (List.fold more
               ~init:(None, empty ())
               ~f:(fun (preve, prev) e ->
                 let fresh () =
                   Fmt.kstr
                     (verbatim ~attr:Attr.(bg (gray 20) ++ fg red))
                     "@[⚠ %s@ %s@]"
                     (if with_date then local_timestamp e.date else "")
                     (Error.blob e)
                   --- prev
                 in
                 ( Some e,
                   match preve with
                   | None -> fresh ()
                   | Some { blob; section; _ }
                     when collapse_similar && String.equal blob e.blob
                          && String.equal section e.section ->
                       prev % verbatim "➕"
                   | Some _ -> fresh () ))
            |> snd))

  let render ?with_date ?collapse_similar self =
    S.bind (render_opt ?with_date ?collapse_similar self) ~f:(function
      | None -> empty ()
      | Some s -> s)
end

module Modal_shortcuts = struct
  module Mode = struct
    type action_result =
      [ `Mode of t | `Home | `Up | `Quit | `Stay | `Prompt of prompt ]

    and prompt = {
      label : string S.t;
      action : string -> action_result;
      initial_value : string;
    }

    and shortcut =
      (* | Idle *)
      | On_key of {
          key : char option;
          name : string S.t;
          action : unit -> action_result;
        }

    and t = { vertical : bool S.t; bindings : shortcut list S.t }

    let entry ?key name ~action = On_key { key; name; action }
    let on_key key name ~action = entry ~key (return name) ~action
    let on_key_dyn key name ~action = entry ~key name ~action
    let just_match name ~action = entry (return name) ~action

    let prompt ~label ?(initial_value = "") action =
      { label; action; initial_value }

    let mode_dyn ?(vertical = return true) bindings = { bindings; vertical }
    let mode ?vertical bindings = mode_dyn ?vertical (return bindings)
  end

  module Modifier = struct
    type t = [ `Ctrl | `Meta | `Shift ]
    [@@deriving sexp, compare, equal, variants, show]
  end

  type t = {
    root : Mode.t; [@main]
    mode_stack : Mode.t list V.t; [@default V.make []]
    input_mode : [ `Single_key | `Match | `Prompting of Mode.prompt ] V.t;
        [@default V.make `Single_key]
    match_text : string V.t; [@default V.make ""]
    errors : Errors.t; [@default Errors.make ()]
    choice : int V.t; [@default V.make 0]
    debug : bool V.t; [@default V.make false]
    modifier : Modifier.t option;
    navigation_bindings : [ `Vimish of Modifier.t ]; [@default `Vimish `Meta]
  }
  [@@deriving fields, make]

  let current_mode self =
    S.map (V.get self.mode_stack) ~f:(function
      | mode :: _tail -> mode
      | [] -> self.root)

  let filter_bindings self =
    let* mode = current_mode self and* match_text = V.get self.match_text in
    let* bindings = mode.bindings in
    let words =
      String.split ~on:' ' match_text
      |> List.filter_map ~f:(fun s ->
             match String.strip s |> String.lowercase with
             | "" -> None
             | s -> Some s)
    in
    List.fold ~init:(return []) bindings ~f:(fun prevm b ->
        let* prev = prevm in
        match b with
        | On_key { name; _ } ->
            let* name = name in
            let name = String.lowercase name in
            if
              List.for_all words ~f:(fun substring ->
                  String.is_substring ~substring name)
            then return (b :: prev)
            else return prev)
    |> S.map ~f:List.rev

  let render ?(scoped_ui = Fn.id) self =
    let logs = V.make [] in
    let log s = V.set logs (s :: V.peek logs) in
    let set_mode_stack v =
      Errors.clear self.errors;
      V.set self.mode_stack v;
      V.set self.match_text "";
      V.set self.input_mode `Single_key;
      V.set self.choice 0
    in
    let run_action action =
      match (action () : Mode.action_result) with
      | `Quit -> assert false
      | `Stay -> set_mode_stack (V.peek self.mode_stack)
      | `Home -> set_mode_stack []
      | `Prompt f ->
          set_mode_stack (V.peek self.mode_stack);
          V.set self.input_mode (`Prompting f);
          V.set self.match_text f.initial_value
      | `Up ->
          set_mode_stack
            (Option.value ~default:[] (List.tl (V.peek self.mode_stack)))
      | `Mode s -> set_mode_stack (s :: V.peek self.mode_stack)
      | exception e -> Errors.add_exn self.errors e
    in
    let of_mode { Mode.vertical; _ } =
      let* vertical = vertical
      and* current_choice = V.get self.choice
      and* input_mode = V.get self.input_mode
      and* bindings = filter_bindings self in
      let run_current_choice () =
        match List.nth bindings current_choice with
        | Some (On_key { action; _ }) -> run_action action
        | None -> Fmt.kstr log "no current choice"
      in
      let show_bindings matching =
        List.mapi bindings ~f:(fun ith -> function
          | On_key { key; name; action } ->
              let* name = name in
              return
              @@ Nottui_widgets.button
                   ~attr:
                     Attr.(
                       Extra_nottui.attr_clickable
                       ++
                       if matching && ith = current_choice then st bold
                       else empty)
                   (Fmt.str "%s%s%s"
                      (match key with
                      | Some k ->
                          Fmt.str "[%s%c] "
                            (match self.modifier with
                            | None -> ""
                            | Some `Meta -> "M-"
                            | Some `Ctrl -> "C-"
                            | Some `Shift -> "S-")
                            k
                      | None -> "")
                      name
                      (if matching && ith = current_choice then " 👈" else ""))
                   (fun () ->
                     run_action action;
                     ()))
      in
      let with_errors ?before l =
        let b = match before with Some b -> fun l -> b :: l | None -> Fn.id in
        S.map (Errors.render_opt self.errors) ~f:(function
          | None -> b l
          | Some e -> b (e :: l))
      in
      let items =
        match input_mode with
        | `Single_key -> with_errors @@ show_bindings false
        | `Match ->
            let before =
              text_input
                (let* s = V.get self.match_text in
                 return (s, String.length s))
                ~on_change:(fun (text, _) -> V.set self.match_text text)
                ~on_submit:(fun _ -> run_current_choice ())
            in
            with_errors ~before (show_bindings true)
        | `Prompting prompt ->
            let before =
              S.bind ~f:bold prompt.label
              %% text_input
                   (let* s = V.get self.match_text in
                    return (s, String.length s))
                   ~on_change:(fun (text, _) -> V.set self.match_text text)
                   ~on_submit:(fun (s, _) ->
                     run_action (fun () -> prompt.action s))
            in
            with_errors ~before []
      in
      let area =
        scoped_ui
          (S.bind items
             ~f:
               (if vertical then vbox
               else fun items -> hbox (List.intersperse ~sep:(space 1 0) items)))
        --- Debug.(
              if_var self.debug @@ fun () ->
              let* l = V.get logs in
              Fmt.kstr verbatim "Mode-debug:\n%a" Fmt.Dump.(list string) l)
      in
      let menu_handler matching (key, mods) =
        match (key, mods, matching, self.navigation_bindings) with
        | `ASCII ' ', [], false, _ ->
            V.set self.input_mode `Match;
            `Handled
        | `ASCII 'l', [ one_mod ], false, `Vimish m when Poly.equal one_mod m ->
            V.set self.input_mode `Match;
            `Handled
        | (`ASCII 'j' | `ASCII 'l'), [ one_mod ], true, `Vimish m
          when Poly.equal one_mod m ->
            V.set self.choice
              (min (List.length bindings - 1) (V.peek self.choice + 1));
            `Handled
        | (`ASCII 'k' | `ASCII 'h'), [ one_mod ], true, `Vimish m
          when Poly.equal one_mod m ->
            V.set self.choice (max 0 (V.peek self.choice - 1));
            `Handled
        | `Escape, [], _, _ ->
            set_mode_stack (if matching then V.peek self.mode_stack else []);
            `Handled
        | `ASCII 'q', [ one_mod ], _, `Vimish m when Poly.equal one_mod m ->
            set_mode_stack (if matching then V.peek self.mode_stack else []);
            `Handled
        | `Enter, [], true, _ ->
            run_current_choice ();
            `Handled
        | `ASCII ascii, mods, false, _
          when match self.modifier with
               | Some m -> List.equal Modifier.equal mods [ m ]
               | None -> Poly.equal mods [] -> (
            match
              List.find bindings ~f:(function On_key { key; _ } ->
                  Option.equal Char.equal key (Some ascii))
            with
            | Some (On_key { action; _ }) ->
                run_action action;
                `Handled
            | None -> `Unhandled)
        | _ -> `Unhandled
      in
      keyboard_area area
        ~handler:
          (match input_mode with
          | `Match -> menu_handler true
          | `Single_key -> menu_handler false
          | `Prompting _ -> (
              function
              | `Escape, [] | `ASCII 'q', [ `Meta ] ->
                  set_mode_stack [];
                  `Handled
              | _ -> `Unhandled))
    in
    S.bind ~f:of_mode (current_mode self)
end

module Run_ui = struct
  let start = Ui_loop.run
end

module Nottui_widgets = struct
  let just_dont_use = ()
end

module Nottui = struct
  let just_dont_use = ()
end

module Notty = struct
  let just_dont_use = ()
end
