open Syb_constructors
open Syb_classes
open Syb_fixpoints

(* Various utilities *)
let unjust l o = match o with Some x -> x :: l | None -> l
let singleton = function [s] -> Some s | _ -> None
let sum l = List.fold_left (+) 0 l
let maximum = List.fold_left max 0


(** Apply a transformation everywhere in bottom-up manner *)
let rec everywhere : genericT -> genericT =
  fun (f : genericT) {X:DATA} x -> f ((gmapT (everywhere f) : genericT) x)

let everywhere_ (f : genericT_) =
  gfixT_ (fun self {X:DATA} x -> f (gmapT_ self x))


(** Apply a transformation everywhere in top-down manner *)
let rec everywhere' : genericT -> genericT =
  fun (f : genericT) {X:DATA} x -> (gmapT (everywhere' f) : genericT) (f x)

let everywhere'_ (f : genericT_) =
  gfixT_ (fun (self : genericT_) {X:DATA} x ->
      let g : genericT_ = (fun {E:DATA} y -> self (f y)) in gmapT_ g x)


(** Variation on everywhere with an extra stop condition *)
let rec everywhereBut : bool genericQ -> genericT -> genericT =
  fun (stop : bool genericQ) (f : genericT) {X:DATA} x ->
    if stop x then x else f ((gmapT (everywhereBut stop f) : genericT) x)

let everywhereBut_ (stop : bool genericQ_) (f : genericT_) =
  gfixT_ (fun self {X:DATA} x ->
      .< if .~(stop x) then .~x else .~(f (gmapT_ self x)) >.)


(** Monadic variation on everywhere *)
(* [TODO]: everywhereM *)


(** Apply a monadic transformation at least somewhere *)
(* [TODO]: somewhere *)


(** Summarise all nodes in top-down, left-to-right order *)
let rec everything : 'r. ('r -> 'r -> 'r) -> 'r genericQ -> 'r genericQ =
  fun (@) g {X: DATA} x ->
    let f = g x in  List.fold_left (@) f (gmapQ (everything (@) g) x)

let everything_ (@) (g : _ genericQ_) =
  gfixQ_ (fun self {X: DATA} x ->
      let f = g x in .< List.fold_left (@) .~f .~(gmapQ_ self x) >.)


(** Variation of "everything" with an added stop condition *)
let rec everythingBut : 'r. ('r -> 'r -> 'r) -> ('r * bool) genericQ -> 'r genericQ =
  fun (@) (stop : _ genericQ) {X: DATA} x ->
    match stop x with
    | v, true -> v
    | v, false -> List.fold_left (@) v (gmapQ (everythingBut (@) stop) x)

let everythingBut_ (@) (stop : _ genericQ_) =
  gfixQ_ (fun self {X: DATA} x ->
      .< match .~(stop x) with
      | v, true -> v
      | v, false -> List.fold_left (@) v .~(gmapQ_ self x) >.)


(** Summarise all nodes in top-down, left-to-right order, carrying some state
    down the tree during the computation, but not left-to-right to
    siblings. *)
(* [TODO]: everythingWithContext *)


(** Get a list of all entities that meet a predicate *)
let listify {R:TYPEABLE} p =
  everything (@) (mkQ [] (fun x ->  if p x then [x] else []))

let listify_ {R:TYPEABLE} p =
  everything_ (@) (mkQ_ .<[]>. (fun x -> .< if .~(p x) then [.~x] else []>.))


(** Look up a subterm by means of a maybe-typed filter *)
(* [TODO]: something *)


(** Bottom-up synthesis of a data structure;
    1st argument z is the initial element for the synthesis;
    2nd argument o is for reduction of results from subterms;
    3rd argument f updates the synthesised data according to the given term
*)
let rec synthesize :
  's 't.'s -> ('t -> 's -> 's) -> ('s -> 't) genericQ -> 't genericQ =
  fun z o (f : _ genericQ) {X: DATA} x ->
   f x (List.fold_right o (gmapQ (synthesize z o f) x) z)

let synthesize_ z o (f : _ genericQ_) =
  gfixQ_ (fun self {X: DATA} x ->
      .< .~(f x) (List.fold_right o .~(gmapQ_ self x) z) >.)


(** Compute size of an arbitrary data structure *)
let rec gsize {D:DATA} v = 1 + sum (gmapQ gsize v)

let gsize_ = gfixQ_ (fun self {D:DATA} v ->
    .< 1 + List.fold_left (+) 0 .~(gmapQ_ self v) >.)


(** Count the number of immediate subterms of the given term *)
let glength {T: DATA} x = List.length (gmapQ (fun {Z:DATA} _ ->  ()) x)

let glength_ {T: DATA} x =
  .< List.length .~(gmapQ_ (fun {Z:DATA} _ -> .< () >.) x) >.


(** Determine depth of the given term *)
let rec gdepth {D: DATA} x = succ (maximum (gmapQ gdepth x))

let gdepth_ = gfixQ_ (fun self {D: DATA} x ->
    .< succ (maximum .~(gmapQ_ self x)) >.)


(** Determine the number of all suitable nodes in a given term *)
let gcount (p : bool genericQ) {T: DATA} x =
  everything (+)
    (fun {X: DATA} x ->  if p x then 1 else 0 )
    x

let gcount_ (p : bool genericQ_) {T: DATA} x =
  everything_ (+)
    (fun {X: DATA} x -> .< if .~(p x) then 1 else 0 >.)
    x


(** Determine the number of all nodes in a given term *)
let gnodecount {X: DATA} x = gcount (fun {Y: DATA} _ ->  true ) x

let gnodecount_ {X: DATA} x = gcount_ (fun {Y: DATA} _ -> .< true >.) x


(** Determine the number of nodes of a given type in a given term *)
let gtypecount {X:TYPEABLE} x = gcount (mkQ  false  (fun _ -> true))

let gtypecount_ {X:TYPEABLE} x = gcount_ (mkQ_ .< false >. (fun _ -> .<true>.))


(** Find (unambiguously) an immediate subterm of a given type *)
let gfindtype {X:TYPEABLE} {D: DATA} x =
   singleton (List.fold_left unjust []
               (gmapQ (mkQ None (fun c -> Some c)) x))

let gfindtype_ {X:TYPEABLE} {D: DATA} x =
  .< singleton (List.fold_left unjust []
               .~(gmapQ_ (mkQ_ .<None>. (fun c -> .<Some .~c>.)) x)) >.


(** Generic show *)
let rec gshow {D:DATA} v =
  string_of_applied_constructor (constructor v)
    (gmapQ gshow v)

let gshow_ = gfixQ_ (fun self {D:DATA} v ->
  .< string_of_applied_constructor .~(constructor_ v)
                                   .~(gmapQ_ self v) >.)

