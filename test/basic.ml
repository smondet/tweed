open! Base
open! Tweed.Std

type t = {
  bpm : int V.t; [@default V.make 120]
  quit : bool V.t; [@default V.make false]
  shortcuts : unit Modal_shortcuts.t option V.t; [@default V.make None]
  data : float V.t; [@default V.make 0.]
  errors : Errors.t; [@default Errors.make ()]
}
[@@deriving fields, make]

let menu self =
  let open Modal_shortcuts.Mode in
  let b_activated = Lwd.var false in
  let vertical = V.make false in
  let rec menu () =
    mode ~vertical:(V.get vertical)
      [
        on_key '=' "increase bpm" ~action:(fun () ->
            V.set self.bpm (V.peek self.bpm + 1);
            `Stay);
        on_key '-' "decrease bpm" ~action:(fun () ->
            V.set self.bpm (max 0 (V.peek self.bpm - 1));
            `Stay);
        on_key 's' "Set the BPM" ~action:(fun () ->
            `Prompt
              (prompt ~label:(return "BPM:")
                 ~initial_value:(Int.to_string (V.peek self.bpm))
                 (fun s ->
                   V.set self.bpm (Int.of_string s);
                   `Stay)));
        on_key 'S' "Set the BPM using $EDITOR" ~action:(fun () ->
            try
              let open Stdlib in
              let tmp = Filename.temp_file "edit-bpm" ".txt" in
              let ret =
                Fmt.kstr Sys.command "echo %d > %s && $EDITOR %s"
                  (V.peek self.bpm) (Filename.quote tmp) (Filename.quote tmp)
              in
              if ret = 0 then (
                let i = open_in tmp in
                match input_line i with
                | s ->
                    V.set self.bpm (int_of_string s);
                    close_in i;
                    `Stay
                | exception _ ->
                    Errors.add self.errors (Fmt.str "Could not read %s" tmp);
                    close_in i;
                    `Stay)
              else (
                Errors.add self.errors (Fmt.str "Response: %d" ret);
                `Stay)
            with e ->
              Errors.add self.errors (Fmt.str "Error: %a" Exn.pp e);
              `Stay);
        on_key 'a' "Submenu" ~action:(fun () ->
            `Mode
              (mode
                 [
                   on_key 'b' "Bbbb home" ~action:(fun () ->
                       Lwd.set b_activated (Lwd.peek b_activated |> not);
                       `Home);
                   on_key 'r' "Recurse" ~action:(fun () -> `Mode (menu ()));
                 ]));
        on_key_dyn 'v'
          S.(
            map (V.get vertical) ~f:(function
              | true -> "Make Horizontal"
              | false -> "Make vertical"))
          ~action:(fun () ->
            V.toggle vertical;
            `Mode (menu ()));
        just_match "Has no key: reset bpm to 120" ~action:(fun () ->
            V.set self.bpm 120;
            `Stay);
        on_key 'q' "Quit" ~action:(fun () ->
            V.set self.quit true;
            `Stay);
      ]
  in
  menu ()

let root self =
  let open Tweed.Std in
  let shortcuts = Modal_shortcuts.(make ~debug:(V.make true) (menu self)) in
  V.set self.shortcuts (Some shortcuts);
  scrollbox
  @@ Modal_shortcuts.(
       render shortcuts (* ~scoped_ui:(fun ui -> scroll_vbox [ ui ]) *))
     --- hspace 2
     --- S.bind (V.get self.bpm) ~f:(Fmt.kstr verbatim "BPM: %d")
     --- hspace 2
     --- S.bind (V.get self.data) ~f:(fun v ->
             (* Fmt.kstr verbatim "Data: %f" v
                %% *)
             Fmt.kstr verbatim "Metronome: %s"
               (String.init
                  (v *. 40. |> Float.to_int)
                  ~f:(function i when Int.(i % 10 = 0) -> '#' | _ -> '=')))
     --- Errors.render self.errors

let () =
  let self = make () in
  let thread () =
    let latest_beat = ref (Unix.gettimeofday ()) in
    while true do
      let now = Unix.gettimeofday () in
      let beat_length = 60. /. Float.of_int (V.peek self.bpm) in
      if Float.(!latest_beat + beat_length <= now) then latest_beat := now;
      V.set self.data ((now -. !latest_beat) /. beat_length);
      Thread.delay 0.01
    done
  in
  let _ = Thread.create thread () in
  Run_ui.start ~quit_on_escape:false ~tick_period:0.05 (root self)
    ~quit:self.quit
