open Higher

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

(* Equality test *)
val (=~~=) : {A:TYPEABLE} -> {B:TYPEABLE} -> (A.t, B.t) eql option

module rec R :
sig
  type    genericT  = {T: R.DATA} -> T.t      -> T.t
  type    genericT_ = {T: R.DATA} -> T.t code -> T.t code
  type 'u genericQ  = {T: R.DATA} -> T.t      -> 'u
  type 'u genericQ_ = {T: R.DATA} -> T.t code -> 'u code
  type 'c genericFapp  =
    < g: 'b. {T: R.DATA} -> (T.t -> 'b, 'c) app -> T.t -> ('b, 'c) app >
  type 'c genericFunit = < u: 'g. 'g -> ('g, 'c) app >
  type 'c genericFapp_  =
    < g: 'b. {T: R.DATA} -> (T.t -> 'b, 'c) app code -> T.t code -> ('b, 'c) app code >
  type 'c genericFunit_ = < u: 'g. 'g code -> ('g, 'c) app code >
  module type DATA =
  sig
    type t
    module Typeable : TYPEABLE with type t = t
    val gmapT  : genericT  -> t      -> t
    val gmapT_ : genericT_ -> t code -> t code
    val gmapQ  : 'u genericQ  -> t      -> 'u list
    val gmapQ_ : 'u genericQ_ -> t code -> 'u list code
    val gfoldl : 'c genericFapp -> 'c genericFunit -> t -> (t, 'c) app
    val gfoldl_ : 'c genericFapp_ -> 'c genericFunit_ -> t code -> (t, 'c) app code
    val constructor : t      -> Syb_constructors.constructor
    val constructor_: t code -> Syb_constructors.constructor code
  end
end
type    genericT  = R.genericT
type    genericT_ = R.genericT_
type 'u genericQ  = 'u R.genericQ
type 'u genericQ_ = 'u R.genericQ_
type 'c genericFapp  = 'c R.genericFapp
type 'c genericFunit = 'c R.genericFunit
type 'c genericFapp_  = 'c R.genericFapp_
type 'c genericFunit_ = 'c R.genericFunit_
                          
module type DATA = R.DATA

val gmapT  : genericT  -> genericT
val gmapT_ : genericT_ -> genericT_

val gmapQ  : 'u genericQ -> 'u list genericQ
val gmapQ_ : 'u genericQ_ -> 'u list genericQ_

val gfoldl  : 'c genericFapp -> 'c genericFunit ->
              {T: DATA} -> T.t -> (T.t, 'c) app
val gfoldl_ : 'c genericFapp_ -> 'c genericFunit_ ->
              {T: DATA} -> T.t code -> (T.t, 'c) app code

val constructor  : Syb_constructors.constructor genericQ
val constructor_ : Syb_constructors.constructor genericQ_

val mkT  : {T:TYPEABLE} -> (T.t -> T.t) -> genericT
val mkT_ : {T:TYPEABLE} -> (T.t code -> T.t code) -> genericT_

val mkQ  : {T:TYPEABLE} -> 'u -> (T.t -> 'u) -> 'u genericQ
val mkQ_ : {T:TYPEABLE} -> 'u code -> (T.t code -> 'u code) -> 'u genericQ_

val generateT : {D:DATA} -> genericT_ -> (D.t -> D.t) code

val generateQ : {D:DATA} -> 'u genericQ_ -> (D.t -> 'u) code

val instantiateT : {D:DATA} -> genericT_ -> D.t -> D.t

val instantiateQ : {D:DATA} -> 'u genericQ_ -> D.t -> 'u
