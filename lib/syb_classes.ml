(* SYB-style equality, using extensible variants to avoid the unsafe cast. *)

(* Equality *)
type (_, _) eql = Refl : ('a, 'a) eql

(* Type representations *)
type _ type_rep = ..

(* Our analogue to the typeable class *)
module type TYPEABLE =
sig
  type t
  val type_rep : unit -> t type_rep
  val eqty : 's type_rep -> (t, 's) eql option
end

let (=~~=) {A: TYPEABLE} {B: TYPEABLE} = A.eqty (B.type_rep ())

(* Implicit instances *)
module rec R :
sig
  type    genericT  = {T: R.DATA} -> T.t      -> T.t
  type    genericT_ = {T: R.DATA} -> T.t code -> T.t code
  type 'u genericQ  = {T: R.DATA} -> T.t      -> 'u
  type 'u genericQ_ = {T: R.DATA} -> T.t code -> 'u code
  module type DATA =
  sig
    type t
    module Typeable : TYPEABLE with type t = t
    val gmapT  : genericT  -> t      -> t
    val gmapT_ : genericT_ -> t code -> t code
    val gmapQ  : 'u genericQ  -> t      -> 'u list
    val gmapQ_ : 'u genericQ_ -> t code -> 'u list code
    val constructor : t      -> Syb_constructors.constructor
    val constructor_: t code -> Syb_constructors.constructor code
  end
end = R
include R

let gmapT f {D: DATA} = D.gmapT f
let gmapT_ f {D: DATA} = D.gmapT_ f

let gmapQ f {D: DATA} = D.gmapQ f
let gmapQ_ f {D: DATA} = D.gmapQ_ f

let constructor {D: DATA} = D.constructor
let constructor_ {D: DATA} = D.constructor_

let app (type a) (type b)
    (module A:TYPEABLE with type t = a)
    (module B:TYPEABLE with type t = b)
    (g : b -> b) (x : a) : a =
  match (=~~=) {A} {B} with
    Some Refl -> g x
  | _ -> x

let app_ (type a) (type b)
    (module A : TYPEABLE with type t = a)
    (module B : TYPEABLE with type t = b)
    (g : b code -> b code) (x : a code) : a code =
  match (=~~=) {A} {B} with
  | Some Refl -> g x
  | _         -> x

let app' (type a) (type b) (type u)
    (module A : TYPEABLE with type t = a)
    (module B : TYPEABLE with type t = b)
    (u:  u) (g : b -> u) (x: a) : u =
  match (=~~=) {A} {B} with
  | Some Refl -> g x
  | _         -> u
    
let app'_ (type a) (type b) (type u)
    (module A : TYPEABLE with type t = a)
    (module B : TYPEABLE with type t = b)
    (u:  u code) (g : b code -> u code) (x: a code) : u code =
  match (=~~=) {A} {B} with
  | Some Refl -> g x
  | _         -> u
    
let mkT : {T:TYPEABLE} -> (T.t -> T.t) -> genericT =
  fun {T:TYPEABLE} g {D: DATA} ->
    app (module D.Typeable) (module T) g

let mkT_ : {T:TYPEABLE} -> (T.t code -> T.t code) -> genericT_ =
  fun {T:TYPEABLE} g {D: DATA} ->
    app_ (module D.Typeable) (module T) g

let mkQ : 'u. {T:TYPEABLE} -> 'u -> (T.t -> 'u) -> 'u genericQ =
  fun {T:TYPEABLE} u g {D: DATA} x ->
    app' (module D.Typeable) (module T) u g x

let mkQ_ : 'u. {T:TYPEABLE} -> 'u code -> (T.t code -> 'u code) -> 'u genericQ_ =
  fun {T:TYPEABLE} u g {D: DATA} x ->
    app'_ (module D.Typeable) (module T) u g x

let generateT {D: DATA} (f : genericT_) =
  Gengenlet.let_locus @@ fun () -> .< fun x -> .~(f .<x>.) >.

let generateQ {D: DATA} (q : 'u genericQ_) =
  Gengenlet.let_locus @@ fun () -> .< fun x -> .~(q .<x>.) >.

let instantiateT {D: DATA} f = Runcode.run (generateT f)

let instantiateQ {D: DATA} q = Runcode.run (generateQ q)
