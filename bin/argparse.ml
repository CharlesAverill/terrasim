type arguments = { num_arg : int }

let parse_arguments () =
  let num_arg = ref 0 in
  let speclist =
    [ ("-n", Arg.Int (fun n -> num_arg := n), "Just a number!") ]
  in
  let usage_msg = "Usage: $PROJECT_NAME -n NUM_ARG" in
  Arg.parse speclist
    (fun n -> print_endline ("Anonymous argument: " ^ n))
    usage_msg;
  { num_arg = !num_arg }
