open Argparse

let () =
  let args = Argparse.parse_arguments () in
  print_endline "Hello, World!"
