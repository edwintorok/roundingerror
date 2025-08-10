open Hardcaml
module Step = Hardcaml_step_testbench.Imperative.Event_driven_sim
module Event_simulator = Step.Simulator.Event_simulator

module Make (I : Interface.S) (O : Interface.S) = struct
  open Hardcaml_event_driven_sim
  module Sim = Make (Two_state_logic)
  module Evsim = With_interface (Two_state_logic) (I) (O)

  let run ~limit create clk testbench =
    let scope =
      Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()
    in
    let waves, {Evsim.simulator= sim; _} =
      Evsim.with_waveterm ~config:Config.trace_all (create scope)
        (fun input output ->
          let input = I.map input ~f:(fun v -> v.signal)
          and output = O.map output ~f:(fun v -> v.signal) in
          let clock = clk input in
          let clock_process =
            Evsim.create_clock clock ~time:1 ~initial_delay:0
          in
          let step_process =
            Step.process () ~clock ~testbench:(testbench input output)
          in
          [clock_process; step_process] )
    in
    Event_simulator.run ~time_limit:(2 * limit) sim ;
    (waves, sim)
end
