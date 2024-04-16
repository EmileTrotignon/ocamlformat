open Import

module Size = struct
  type t =
    | Small
    | Large
end

let[@inline never] test (type t) m size =
  let (module T : Int.S with type t = t) = m in
  let fwd =
    match (size : Size.t) with
    | Small -> List.map ~f:T.of_int_exn [ 0; -20; -10; -1; 1; 2; 4; 100 ]
    | Large -> [ T.min_value; T.max_value; T.zero ]
  in
  let rev = List.rev fwd in
  List.iter2_exn fwd rev ~f:(fun a b ->
    print_s [%message "min" ~_:(a : T.t) ~_:(b : T.t) "=" ~_:(T.min a b : T.t)];
    print_s [%message "max" ~_:(a : T.t) ~_:(b : T.t) "=" ~_:(T.max a b : T.t)])
;;

let%expect_test "small values" =
  let test (module T : Int.S) =
    test (module T) Small;
    [%expect
      {|
      (min 0 100 = 0)
      (max 0 100 = 100)
      (min -20 4 = -20)
      (max -20 4 = 4)
      (min -10 2 = -10)
      (max -10 2 = 2)
      (min -1 1 = -1)
      (max -1 1 = 1)
      (min 1 -1 = -1)
      (max 1 -1 = 1)
      (min 2 -10 = -10)
      (max 2 -10 = 2)
      (min 4 -20 = -20)
      (max 4 -20 = 4)
      (min 100 0 = 0)
      (max 100 0 = 100) |}]
  in
  test (module Int);
  test (module Int64);
  test (module Int32);
  test (module Nativeint);
  [%expect {| |}]
;;

let%expect_test "fixed-size types" =
  test (module Int64) Large;
  [%expect
    {|
    (min -9_223_372_036_854_775_808 0 = -9_223_372_036_854_775_808)
    (max -9_223_372_036_854_775_808 0 = 0)
    (min
     9_223_372_036_854_775_807
     9_223_372_036_854_775_807
     =
     9_223_372_036_854_775_807)
    (max
     9_223_372_036_854_775_807
     9_223_372_036_854_775_807
     =
     9_223_372_036_854_775_807)
    (min 0 -9_223_372_036_854_775_808 = -9_223_372_036_854_775_808)
    (max 0 -9_223_372_036_854_775_808 = 0) |}];
  test (module Int32) Large;
  [%expect
    {|
    (min -2_147_483_648 0 = -2_147_483_648)
    (max -2_147_483_648 0 = 0)
    (min 2_147_483_647 2_147_483_647 = 2_147_483_647)
    (max 2_147_483_647 2_147_483_647 = 2_147_483_647)
    (min 0 -2_147_483_648 = -2_147_483_648)
    (max 0 -2_147_483_648 = 0) |}]
;;

let%expect_test ("64-bit platforms" [@tags "no-js", "64-bits-only"]) =
  test (module Int) Large;
  [%expect
    {|
    (min -4_611_686_018_427_387_904 0 = -4_611_686_018_427_387_904)
    (max -4_611_686_018_427_387_904 0 = 0)
    (min
     4_611_686_018_427_387_903
     4_611_686_018_427_387_903
     =
     4_611_686_018_427_387_903)
    (max
     4_611_686_018_427_387_903
     4_611_686_018_427_387_903
     =
     4_611_686_018_427_387_903)
    (min 0 -4_611_686_018_427_387_904 = -4_611_686_018_427_387_904)
    (max 0 -4_611_686_018_427_387_904 = 0) |}];
  test (module Nativeint) Large;
  [%expect
    {|
    (min -9_223_372_036_854_775_808 0 = -9_223_372_036_854_775_808)
    (max -9_223_372_036_854_775_808 0 = 0)
    (min
     9_223_372_036_854_775_807
     9_223_372_036_854_775_807
     =
     9_223_372_036_854_775_807)
    (max
     9_223_372_036_854_775_807
     9_223_372_036_854_775_807
     =
     9_223_372_036_854_775_807)
    (min 0 -9_223_372_036_854_775_808 = -9_223_372_036_854_775_808)
    (max 0 -9_223_372_036_854_775_808 = 0) |}]
;;

let%expect_test ("32-bit platforms" [@tags "no-js", "32-bits-only"]) =
  test (module Int) Large;
  [%expect
    {|
    (min -1_073_741_824 0 = -1_073_741_824)
    (max -1_073_741_824 0 = 0)
    (min 1_073_741_823 1_073_741_823 = 1_073_741_823)
    (max 1_073_741_823 1_073_741_823 = 1_073_741_823)
    (min 0 -1_073_741_824 = -1_073_741_824)
    (max 0 -1_073_741_824 = 0) |}];
  test (module Nativeint) Large;
  [%expect
    {|
    (min -2_147_483_648 0 = -2_147_483_648)
    (max -2_147_483_648 0 = 0)
    (min 2_147_483_647 2_147_483_647 = 2_147_483_647)
    (max 2_147_483_647 2_147_483_647 = 2_147_483_647)
    (min 0 -2_147_483_648 = -2_147_483_648)
    (max 0 -2_147_483_648 = 0) |}]
;;

let%expect_test ("js_of_ocaml platforms" [@tags "js-only"]) =
  test (module Int) Large;
  [%expect
    {|
    (min -2_147_483_648 0 = -2_147_483_648)
    (max -2_147_483_648 0 = 0)
    (min 2_147_483_647 2_147_483_647 = 2_147_483_647)
    (max 2_147_483_647 2_147_483_647 = 2_147_483_647)
    (min 0 -2_147_483_648 = -2_147_483_648)
    (max 0 -2_147_483_648 = 0) |}];
  test (module Nativeint) Large;
  [%expect
    {|
    (min -2_147_483_648 0 = -2_147_483_648)
    (max -2_147_483_648 0 = 0)
    (min 2_147_483_647 2_147_483_647 = 2_147_483_647)
    (max 2_147_483_647 2_147_483_647 = 2_147_483_647)
    (min 0 -2_147_483_648 = -2_147_483_648)
    (max 0 -2_147_483_648 = 0) |}]
;;
