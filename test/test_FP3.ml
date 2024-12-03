let () =
  Alcotest.run
    "FP3 Tests"
    [ "Interpolation", Test_interpolation.interpolation_tests
    ; "Runner", Test_runner.runner_tests
    ]
;;
