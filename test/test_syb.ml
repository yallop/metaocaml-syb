open OUnit2
open Syb_classes
open Syb_schemes
open Syb_instances

let test_gshow _ =
  assert_equal ~printer:(fun x -> x)
    "((true, 1) :: ((false, 2) :: ((false, 3) :: [])))"
    (gshow
       [(true, 1);
        (false, 2);
        (false, 3)])

let test_gshow_ _ =
  assert_equal ~printer:(fun x -> x)
    "((true, 1) :: ((false, 2) :: ((false, 3) :: [])))"
    (instantiateQ gshow_
       [(true, 1);
        (false, 2);
        (false, 3)])

let test_gsize _ =
  assert_equal ~printer:string_of_int
    13
    (gsize
       [(true, 1);
        (false, 2);
        (false, 3)])

let test_gsize_ _ =
  assert_equal ~printer:string_of_int
    13
    (instantiateQ gsize_
       [(true, 1);
        (false, 2);
        (false, 3)])

let test_everywhere _ =
  assert_equal
    [(false, 1);
     (true, 2);
     (true, 3)]
    ((everywhere (mkT not))
       [(true, 1);
        (false, 2);
        (false, 3)])
  
let test_everywhere_ _ =
  assert_equal
    [(false, 1);
     (true, 2);
     (true, 3)]
    ((instantiateT (everywhere_ (mkT_ (fun x -> .<not .~x >.))))
       [(true, 1);
        (false, 2);
        (false, 3)])
  
let test_everywhere' _ =
  assert_equal
    [(false, 1);
     (true, 2);
     (true, 3)]
    ((everywhere' (mkT not))
       [(true, 1);
        (false, 2);
        (false, 3)])
  
let test_everywhere'_ _ =
  assert_equal
    [(false, 1);
     (true, 2);
     (true, 3)]
    ((instantiateT (everywhere'_ (mkT_ (fun x -> .<not .~x>.))))
       [(true, 1);
        (false, 2);
        (false, 3)])

let test_everything _ =
  let ints_gt_0 = mkQ [] (fun x -> if x > 0 then [x] else []) in
  assert_equal
    [1; 2; 3; 20]
    ((everything (@) ints_gt_0)
    [(false, 1);
     (true, 2);
     (true, 3);
     (true, -10);
     (false, 20);
    ])

let test_everything_ _ =
  let ints_gt_0 = mkQ_ .<[]>. (fun x -> .<if .~x > 0 then [.~x] else []>.) in
  assert_equal
    [1; 2; 3; 20]
    ((instantiateQ (everything_ (@) ints_gt_0))
    [(false, 1);
     (true, 2);
     (true, 3);
     (true, -10);
     (false, 20);
    ])

let test_instantiate_everywhere_without_function _ =
  assert_equal
    [(false, 2);
     (true,  3);
     (false, 4)]
    (everywhere (mkT succ)
       [(false, 1);
        (true,  2);
        (false, 3)])

let test_instantiate_everywhere_without_function_ _ =
  assert_equal
    [(false, 2);
     (true,  3);
     (false, 4)]
    (instantiateT (everywhere_ (mkT_ (fun x -> .<succ .~x>.)))
       [(false, 1);
        (true,  2);
        (false, 3)])

let suite = "SYB tests" >:::
  ["gshow"
    >:: test_gshow;

   "gshow_"
    >:: test_gshow_;

    "gsize"
    >:: test_gsize;

    "gsize_"
    >:: test_gsize_;

    "everywhere"
    >:: test_everywhere;

    "everywhere_"
    >:: test_everywhere_;

    "everywhere'"
    >:: test_everywhere';

    "everywhere'_"
    >:: test_everywhere'_;

    "everything"
    >:: test_everything;

    "everything_"
    >:: test_everything_;

    "everything without function"
    >:: test_instantiate_everywhere_without_function;

    "everything without function_"
    >:: test_instantiate_everywhere_without_function_;
  ]


let _ =
  run_test_tt_main suite
