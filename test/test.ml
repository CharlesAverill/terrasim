(* Functions to test *)
module To_test = struct
  let id x = x
  let add5 x = x + 5
end

let test_id () =
  Alcotest.(check bool)
    "same bool" true
    ("hello world" = To_test.id "hello world")

let test_add5 () = Alcotest.(check int) "same int" 27 (To_test.add5 22)

(* Run tests *)
let () =
  let open Alcotest in
  run "Tests"
    [
      ("Identity Function", [ test_case "id" `Quick test_id ]);
      ("Add5 Function", [ test_case "add5" `Quick test_add5 ]);
    ]
