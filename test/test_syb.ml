open OUnit2
open Higher
open Syb_classes
open Syb_schemes
open Syb_instances
open Syb_fixpoints

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
    ((instantiateQ (everything_ (fun x y -> .< .~x @ .~y >.) ints_gt_0))
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

let test_gfoldl_gmap _ =
  let module Definitions =
  struct
    module Id = Newtype1(struct type 'a t = 'a end)
    (* gmapT in terms of gfoldl *)
    let gmapT (f : genericT) : genericT =
      let f : _ genericFapp =
        object
          method g: 'b. {T: R.DATA} -> (T.t -> 'b, 'c) app -> T.t -> ('b, 'c) app =
            fun {T: R.DATA} g x -> Id.inj (Id.prj g (f x))
        end
      and u : _ genericFunit =
        object
          method u: 'g. 'g -> ('g, 'c) app = Id.inj
        end in
      fun {D:DATA} (x: D.t) -> Id.prj (D.gfoldl f u x)

    let rec everywhere : genericT -> genericT =
      fun (f : genericT) {X:DATA} x -> f ((gmapT (everywhere f) : genericT) x)
  end
  in
  assert_equal
    [(false, 1);
     (true, 2);
     (true, 3)]
    ((Definitions.everywhere (mkT not))
       [(true, 1);
        (false, 2);
        (false, 3)])


let test_gfoldl_gmap_ _ =
  let module Definitions =
  struct
    module Id = Newtype1(struct type 'a t = 'a end)
    (* gmapT_ in terms of gfoldl_ *)
    let gmapT_ (f : genericT_) : genericT_ =
      let f : _ genericFapp_  =
        object 
          method g: 'b. {T: R.DATA} -> (T.t -> 'b, 'c) app code -> T.t code -> ('b, 'c) app code
            = fun {T: R.DATA} g x -> .< Id.inj (Id.prj .~g .~(f x)) >.
        end
      and u : _ genericFunit_ =
        object
          method u: 'g. 'g code -> ('g, 'c) app code
            = fun g -> .< Id.inj .~g >.
        end in
      fun {D:DATA} (x: D.t code) -> .<Id.prj .~(D.gfoldl_ f u x) >.

    let everywhere_ (f : genericT_) =
      gfixT_ (fun self {X:DATA} x -> f (gmapT_ self x))
  end
  in
  assert_equal
    [(false, 1);
     (true, 2);
     (true, 3)]
    ((instantiateT (Definitions.everywhere_ (mkT_ (fun x -> .<not .~x >.))))
       [(true, 1);
        (false, 2);
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

    "everywhere using gfoldl"
    >:: test_gfoldl_gmap;

    "everywhere_ using gfoldl_"
    >:: test_gfoldl_gmap_;
  ]


let _ =
  run_test_tt_main suite
