type arguments = { test_noise : bool; verbosity : int }

let parse_arguments () =
  let test_noise = ref false in
  let verbose = ref 1 in
  let speclist =
    [
      ("--test-noise", Arg.Set test_noise, "Test terrasim's noise generator");
      ( "--verbose",
        Arg.Set_int verbose,
        Printf.sprintf
          "Set verbosity level (defaults to %d): [(0, None), (1, Debug), (2, \
           Info), (3, Warning), (4, Error), (5, Critical)]"
          !verbose );
    ]
  in
  let usage_msg = "Usage: terrasim [--test-noise|--verbose V]" in
  Arg.parse speclist
    (fun n -> print_endline ("Anonymous argument: " ^ n))
    usage_msg;
  { test_noise = !test_noise; verbosity = !verbose }
