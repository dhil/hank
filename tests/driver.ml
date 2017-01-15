let passing =
  (*QCheck.Test.make ~count:1000
    ~name:"list_rev_is_involutive"
    QCheck.(list small_int)
    (fun l -> List.rev (List.rev l) = l);;*)
  Cp_test.tests 100

let failing =
  QCheck.Test.make ~count:10
    ~name:"fail_sort_id"
    QCheck.(list small_int)
    (fun l -> l = List.sort compare l);;

let suite =
  let open OUnit2 in
  "suite" >:::
  ["charparser" >:::
     List.map QCheck_runner.to_ounit2_test passing]
    
let _ =
  OUnit2.run_test_tt_main suite
    
