open Hardcaml
open Sexplib.Std
open Config
module WT = Hardcaml_event_driven_sim.Waveterm

module I = struct
  type 'a t = {clk: 'a; rst_n: 'a; test: 'a} [@@deriving sexp_of, hardcaml]
end

module Controller = struct
  module I = I

  module O = struct
    type 'a t =
      { hsync: 'a  (** horizontal sync *)
      ; vsync: 'a  (** vertical sync *)
      ; coord: 'a Coord.t
      ; blank: 'a
            (** outside of active video zone. rgb has to be forced to 0. *) }
    [@@deriving sexp_of, hardcaml]
  end

  open Signal

  let reg I.{clk; rst_n; _} =
    (* ASIC (not FPGA) optimized, use asynchronous clear, instead of synchronous reset *)
    Reg_spec.(
      create ~clock:clk () |> override ~reset:rst_n ~reset_edge:Edge.Falling )

  let create ~modeline _scope input =
    let open Modeline in
    (* TODO: clock divider support *)
    let r_sync = reg input in
    let enable = vdd in
    let h =
      Timing.cnt_sync modeline.horiz r_sync ~reg_width:Config.hbits ~enable
    in
    let v =
      (* vsync starts at same time as hsync,
         see "3.5 DMT Video Timing Parameter Definitions - Total Frame Timing"
      *)
      Timing.cnt_sync modeline.vert r_sync ~reg_width:Config.vbits
        ~enable:h.sync_start
    in
    (* we blank the border too, although this is not required *)
    let blank =
      ~:( h.cnt <:. modeline.horiz.addressable
        &: (v.cnt <:. modeline.vert.addressable) )
    in
    let coord = Coord.{x= h.cnt; y= v.cnt} in
    O.{hsync= h.sync; vsync= v.sync; blank; coord}

  let hierarchical ~modeline scope ?instance input =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"vgaController" ?instance (create ~modeline)
      input
end

let display_rules =
  let of_bits bits = Bits.to_int bits |> Printf.sprintf "%X" in
  let f _port = Some Wave_format.(Bit_or (Custom of_bits)) in
  [Hardcaml_waveterm.Display_rule.custom ~f]

let%expect_test "hvsync" =
  let open Expect_test_helpers_base in
  let modeline = Modeline.test_config in
  let module C = Controller in
  let module W = Waveforms.Make (C.I) (C.O) in
  let h = Modeline.Timing.to_modeline_pos modeline.horiz
  and v = Modeline.Timing.to_modeline_pos modeline.vert in
  let monitor = Array.make_matrix v.total h.total "x"
  and hsyncs = Array.make_matrix v.total h.total "x"
  and vsyncs = Array.make_matrix v.total h.total "x" in
  let waves, _ =
    W.run
      ~limit:(h.total * v.total * 2)
      (C.create ~modeline)
      (fun i -> i.C.I.clk)
    @@ fun inputs outputs () ->
    let open Waveforms in
    let open Step.Let_syntax in
    let ( <-- ) = Event_simulator.( <-- ) in
    inputs.rst_n <-- Bits.gnd ;
    let%bind () = Step.cycle () in
    let%bind () = Step.cycle () in
    let%bind () = Step.cycle () in
    inputs.rst_n <-- Bits.vdd ;
    Step.for_ 0 (h.total * v.total * 2)
    @@ fun i ->
    (* Our counter starts at 0 = beginning of active region.
       However the monitor starts drawing at Hsync+Vsync start.
       To show how the signal looks like for the monitor we offset.
    *)
    let offset = h.sync_start + (h.total * (v.sync_start - 1)) in
    let hpos = (i - offset) mod h.total
    and vpos = (i - offset) / h.total mod v.total in
    let open WT.Sim.Signal in
    let hsync = Bits.to_bool (read outputs.hsync)
    and vsync = Bits.to_bool (read outputs.vsync)
    and blank = Bits.to_bool (read outputs.blank)
    and hcnt = Bits.to_int (read outputs.coord.x)
    and vcnt = Bits.to_int (read outputs.coord.y) in
    let str = if blank then "B" else Printf.sprintf "%2u,%2u" vcnt hcnt in
    if i >= offset && vpos < v.total && hpos < h.total then (
      hsyncs.(vpos).(hpos) <- (if hsync then "H" else " ") ;
      vsyncs.(vpos).(hpos) <- (if vsync then "V" else " ") ;
      monitor.(vpos).(hpos) <- Printf.sprintf "%5s" str ) ;
    let%bind () = Step.cycle () in
    return ()
    (* clock is automatically set, no need to change it *)
  in
  print_endline (Modeline.to_modeline modeline) ;
  print_endline "vga1:" ;
  WT.Waveform.expect ~display_rules ~wave_width:(-1) ~serialize_to:"vga1"
    ~display_height:22 ~display_width:76 waves ;
  print_endline "vga2:" ;
  WT.Waveform.expect ~display_rules ~wave_width:(-1) ~serialize_to:"vga2"
    ~start_cycle:(2 * ((h.total * (v.sync_start - 1)) + h.sync_start))
    ~display_height:22 ~display_width:76 waves ;
  print_endline "vga3:" ;
  WT.Waveform.expect ~display_rules ~wave_width:(-2) ~serialize_to:"vga3"
    ~start_cycle:(2 * (h.total * (v.sync_start - 1)))
    ~display_height:22 ~display_width:76 waves ;
  print_endline "vga4:" ;
  WT.Waveform.expect ~display_rules ~wave_width:(-2) ~serialize_to:"vga4"
    ~start_cycle:(2 * ((h.total * (v.disp - 1)) + h.disp - 2))
    ~display_height:22 ~display_width:76 waves ;
  print_endline "vga5:" ;
  WT.Waveform.expect ~display_rules ~wave_width:(-2) ~serialize_to:"vga5"
    ~start_cycle:(2 * (h.total * v.sync_start))
    ~display_height:22 ~display_width:76 waves ;
  let draw_monitor a =
    print_endline "" ;
    String.concat "\n"
      ( a |> Array.to_list
      |> List.map
         @@ fun row ->
         ["|"; row |> Array.to_list |> String.concat "|"; "|"]
         |> String.concat "" )
    |> print_endline
  in
  print_endline "hsyncs:" ;
  let h = modeline.horiz in
  print_s [%message (h : Modeline.Timing.t)] ;
  draw_monitor hsyncs ;
  print_endline "vsyncs:" ;
  let v = modeline.vert in
  print_s [%message (v : Modeline.Timing.t)] ;
  draw_monitor vsyncs ;
  print_endline "blanks:" ;
  let blank_hl, blank_hr = (h.sync + h.back_porch, h.front_porch)
  and blank_vt, blank_vb = (v.sync + v.back_porch, v.front_porch) in
  print_s
    [%message
      (blank_hl : int) (blank_hr : int) (blank_vt : int) (blank_vb : int)] ;
  draw_monitor monitor ;
  [%expect
    {|
    ModeLine "2x8_59.52" 0.010 2 5 8 12 8 11 13 14 +hsync +vsync
    vga1:
    ┌Signals──────────┐┌Waves──────────────────────────────────────────────────┐
    │blank            ││      ┌───────────────────┐   ┌───────────────────┐   ┌│
    │                 ││──────┘                   └───┘                   └───┘│
    │clk              ││─┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌│
    │                 ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘│
    │hsync            ││            ┌─────┐                 ┌─────┐            │
    │                 ││────────────┘     └─────────────────┘     └────────────│
    │rst_n            ││    ┌──────────────────────────────────────────────────│
    │                 ││────┘                                                  │
    │test             ││                                                       │
    │                 ││───────────────────────────────────────────────────────│
    │vdd              ││───────────────────────────────────────────────────────│
    │                 ││                                                       │
    │vsync            ││                                                       │
    │                 ││───────────────────────────────────────────────────────│
    │                 ││────┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬│
    │x                ││ 0  │1│2│3│4│5│6│7│8│9│A│B│0│1│2│3│4│5│6│7│8│9│A│B│0│1││
    │                 ││────┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴│
    │                 ││────────────┬───────────────────────┬──────────────────│
    │y                ││ 0          │1                      │2                 │
    │                 ││────────────┴───────────────────────┴──────────────────│
    └─────────────────┘└───────────────────────────────────────────────────────┘
    bc0e25402fd63d4e6af23cc11a2096ea
    vga2:
    ┌Signals──────────┐┌Waves──────────────────────────────────────────────────┐
    │blank            ││───────────────────────────────────────────────────────│
    │                 ││                                                       │
    │clk              ││─┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌│
    │                 ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘│
    │hsync            ││  ┌─────┐                 ┌─────┐                 ┌────│
    │                 ││──┘     └─────────────────┘     └─────────────────┘    │
    │rst_n            ││───────────────────────────────────────────────────────│
    │                 ││                                                       │
    │test             ││                                                       │
    │                 ││───────────────────────────────────────────────────────│
    │vdd              ││───────────────────────────────────────────────────────│
    │                 ││                                                       │
    │vsync            ││  ┌───────────────────────────────────────────────┐    │
    │                 ││──┘                                               └────│
    │                 ││──┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬│
    │x                ││ 4│5│6│7│8│9│A│B│0│1│2│3│4│5│6│7│8│9│A│B│0│1│2│3│4│5│6││
    │                 ││──┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴│
    │                 ││──┬───────────────────────┬───────────────────────┬────│
    │y                ││ A│B                      │C                      │D   │
    │                 ││──┴───────────────────────┴───────────────────────┴────│
    └─────────────────┘└───────────────────────────────────────────────────────┘
    bc0e25402fd63d4e6af23cc11a2096ea
    vga3:
    ┌Signals──────────┐┌Waves──────────────────────────────────────────────────┐
    │blank            ││─────────────────────────────────────────────────┐ ┌───│
    │                 ││                                                 └─┘   │
    │clk              ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
    │                 ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
    │hsync            ││      ┌──┐        ┌──┐        ┌──┐        ┌──┐        ┌│
    │                 ││──────┘  └────────┘  └────────┘  └────────┘  └────────┘│
    │rst_n            ││───────────────────────────────────────────────────────│
    │                 ││                                                       │
    │test             ││                                                       │
    │                 ││───────────────────────────────────────────────────────│
    │vdd              ││───────────────────────────────────────────────────────│
    │                 ││                                                       │
    │vsync            ││      ┌───────────────────────┐                        │
    │                 ││──────┘                       └────────────────────────│
    │                 ││─┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬│
    │x                ││ │││││││││││││││││││││││││││││││││││││││││││││││││││││││
    │                 ││─┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴│
    │                 ││──────┬───────────┬───────────┬───────────┬───────────┬│
    │y                ││ A    │B          │C          │D          │0          ││
    │                 ││──────┴───────────┴───────────┴───────────┴───────────┴│
    └─────────────────┘└───────────────────────────────────────────────────────┘
    bc0e25402fd63d4e6af23cc11a2096ea
    vga4:
    ┌Signals──────────┐┌Waves──────────────────────────────────────────────────┐
    │blank            ││─┐ ┌───────────────────────────────────────────────────│
    │                 ││ └─┘                                                   │
    │clk              ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
    │                 ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
    │hsync            ││      ┌──┐        ┌──┐        ┌──┐        ┌──┐        ┌│
    │                 ││──────┘  └────────┘  └────────┘  └────────┘  └────────┘│
    │rst_n            ││───────────────────────────────────────────────────────│
    │                 ││                                                       │
    │test             ││                                                       │
    │                 ││───────────────────────────────────────────────────────│
    │vdd              ││───────────────────────────────────────────────────────│
    │                 ││                                                       │
    │vsync            ││                                          ┌────────────│
    │                 ││──────────────────────────────────────────┘            │
    │                 ││─┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬│
    │x                ││ │││││││││││││││││││││││││││││││││││││││││││││││││││││││
    │                 ││─┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴│
    │                 ││──────┬───────────┬───────────┬───────────┬───────────┬│
    │y                ││ 7    │8          │9          │A          │B          ││
    │                 ││──────┴───────────┴───────────┴───────────┴───────────┴│
    └─────────────────┘└───────────────────────────────────────────────────────┘
    bc0e25402fd63d4e6af23cc11a2096ea
    vga5:
    ┌Signals──────────┐┌Waves──────────────────────────────────────────────────┐
    │blank            ││─────────────────────────────────────┐ ┌─────────┐ ┌───│
    │                 ││                                     └─┘         └─┘   │
    │clk              ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
    │                 ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
    │hsync            ││      ┌──┐        ┌──┐        ┌──┐        ┌──┐        ┌│
    │                 ││──────┘  └────────┘  └────────┘  └────────┘  └────────┘│
    │rst_n            ││───────────────────────────────────────────────────────│
    │                 ││                                                       │
    │test             ││                                                       │
    │                 ││───────────────────────────────────────────────────────│
    │vdd              ││───────────────────────────────────────────────────────│
    │                 ││                                                       │
    │vsync            ││──────────────────┐                                    │
    │                 ││                  └────────────────────────────────────│
    │                 ││─┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬│
    │x                ││ │││││││││││││││││││││││││││││││││││││││││││││││││││││││
    │                 ││─┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴│
    │                 ││──────┬───────────┬───────────┬───────────┬───────────┬│
    │y                ││ B    │C          │D          │0          │1          ││
    │                 ││──────┴───────────┴───────────┴───────────┴───────────┴│
    └─────────────────┘└───────────────────────────────────────────────────────┘
    bc0e25402fd63d4e6af23cc11a2096ea
    hsyncs:
    (h (
      (sync        3)
      (polarity    High)
      (back_porch  4)
      (border      0)
      (addressable 2)
      (front_porch 3)))

    |H|H|H| | | | | | | | | |
    |H|H|H| | | | | | | | | |
    |H|H|H| | | | | | | | | |
    |H|H|H| | | | | | | | | |
    |H|H|H| | | | | | | | | |
    |H|H|H| | | | | | | | | |
    |H|H|H| | | | | | | | | |
    |H|H|H| | | | | | | | | |
    |H|H|H| | | | | | | | | |
    |H|H|H| | | | | | | | | |
    |H|H|H| | | | | | | | | |
    |H|H|H| | | | | | | | | |
    |H|H|H| | | | | | | | | |
    |H|H|H| | | | | | | | | |
    vsyncs:
    (v (
      (sync        2)
      (polarity    High)
      (back_porch  1)
      (border      0)
      (addressable 8)
      (front_porch 3)))

    |V|V|V|V|V|V|V|V|V|V|V|V|
    |V|V|V|V|V|V|V|V|V|V|V|V|
    | | | | | | | | | | | | |
    | | | | | | | | | | | | |
    | | | | | | | | | | | | |
    | | | | | | | | | | | | |
    | | | | | | | | | | | | |
    | | | | | | | | | | | | |
    | | | | | | | | | | | | |
    | | | | | | | | | | | | |
    | | | | | | | | | | | | |
    | | | | | | | | | | | | |
    | | | | | | | | | | | | |
    | | | | | | | | | | | | |
    blanks:
    ((blank_hl 7)
     (blank_hr 3)
     (blank_vt 3)
     (blank_vb 3))

    |    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|
    |    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|
    |    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|
    |    B|    B|    B|    B|    B|    B|    B| 0, 0| 0, 1|    B|    B|    B|
    |    B|    B|    B|    B|    B|    B|    B| 1, 0| 1, 1|    B|    B|    B|
    |    B|    B|    B|    B|    B|    B|    B| 2, 0| 2, 1|    B|    B|    B|
    |    B|    B|    B|    B|    B|    B|    B| 3, 0| 3, 1|    B|    B|    B|
    |    B|    B|    B|    B|    B|    B|    B| 4, 0| 4, 1|    B|    B|    B|
    |    B|    B|    B|    B|    B|    B|    B| 5, 0| 5, 1|    B|    B|    B|
    |    B|    B|    B|    B|    B|    B|    B| 6, 0| 6, 1|    B|    B|    B|
    |    B|    B|    B|    B|    B|    B|    B| 7, 0| 7, 1|    B|    B|    B|
    |    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|
    |    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|
    |    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|    B|
    |}]

module O = struct
  type 'a t = {srgb: 'a SRGB.t; hsync: 'a; vsync: 'a}
  [@@deriving sexp_of, hardcaml]
end

let linear_to_srgb1 linear =
  let n = (1 lsl Config.Linear.bpp) - 1 in
  let out_bits = Config.SRGB.bpp in
  let out_max = (1 lsl out_bits) - 1 in
  (* TODO: ROM with 3 read ports *)
  Signal.mux_init linear (n + 1) ~f:(fun i ->
      let f = float i /. float n in
      let srgb, _, _, _ = Gg.(Color.(v f f f 1. |> to_srgb) |> V4.to_tuple) in
      let v = srgb *. float out_max |> Float.round |> Float.to_int in
      v |> Signal.of_int ~width:out_bits )

let linear_to_srgb Linear.{lr; lg; lb} =
  SRGB.{r= linear_to_srgb1 lr; g= linear_to_srgb1 lg; b= linear_to_srgb1 lb}

type 'a image = Scope.t -> 'a ImageIn.t -> 'a Linear.t

let create ~image ~modeline scope input =
  let controller = Controller.hierarchical ~modeline scope input in
  let clk = input.clk and rst_n = input.rst_n in
  let open Controller.O in
  let rgb = image scope ImageIn.{clk; coord= controller.coord; rst_n} in
  let z = Signal.zero Config.SRGB.bpp in
  let zero = SRGB.{r= z; g= z; b= z} in
  let srgb =
    rgb |> linear_to_srgb |> SRGB.Of_signal.mux2 controller.blank zero
  in
  O.{hsync= controller.hsync; vsync= controller.vsync; srgb}
  (* add register on output for all signals: helps with timing analysis,
     and avoiding glitches on the output *)
  |> O.Of_signal.reg (Controller.reg input)

let hierarchical ~modeline ~image scope ?instance input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"vga" (create ~modeline ~image) input
