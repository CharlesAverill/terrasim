type arguments = { test_noise : bool }

let parse_arguments () =
  let test_noise = ref false in
  let speclist =
    [ ("--test-noise", Arg.Set test_noise, "Test terrasim's noise generator") ]
  in
  let usage_msg = "Usage: terrasim [--test-noise]" in
  Arg.parse speclist
    (fun n -> print_endline ("Anonymous argument: " ^ n))
    usage_msg;
  { test_noise = !test_noise }
