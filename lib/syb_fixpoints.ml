open Syb_classes

let letrec k =
  let r = Gengenlet.genlet (.< ref (fun _ -> assert false) >.) in
  let _ : unit code = Gengenlet.genlet (.<.~r := .~(k .< ! .~r >.) >.) in
  .< ! .~r >.

implicit module Typeable_of_data{A:DATA} = Syb_instances.Typeable_of_data(A)

module Memofix  (Elem: sig type (_,_) t end) =
struct
  type _ t =
      Nil : 'a t
    | Cons : (module TYPEABLE with type t = 'b) * ('b -> ('a, 'b) Elem.t) code * 'a t -> 'a t

  let empty () = ref Nil

  let add {T:TYPEABLE} t c = t := Cons ((module T), c, !t)

  let rec lookup : type a. {T:TYPEABLE} -> a t -> (T.t -> (a, T.t) Elem.t) code option =
    fun {T: TYPEABLE} -> function
        Nil -> None
      | Cons ((module R), d, rest) ->
        match (=~~=) {T} {R} with
          Some Refl -> Some d
        | None -> lookup rest

  type 'a f = {T: DATA} -> T.t code -> ('a, T.t) Elem.t code

  let memofix (openf : 'v f -> 'v f) =
    let tbl = empty () in
    let rec result {D: DATA} x =
      match lookup !tbl with
        Some g -> .< .~g .~x >.
      | None -> let g = letrec (fun self ->
                                  add tbl self;
                                  .< fun y -> .~(openf result .<y>.) >.)
                 in .< .~g .~x >.
    in result
end

module T = Memofix(struct type ('a, 't) t = 't end)
let gfixT_ = T.memofix

module Q = Memofix(struct type ('a, 't) t = 'a end)
let gfixQ_ = Q.memofix
