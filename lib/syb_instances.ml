open Syb_constructors
open Syb_classes

(* Some primitive typeable instances *)
type _ type_rep += List : 'a type_rep -> 'a list type_rep
type _ type_rep += Option : 'a type_rep -> 'a option type_rep
type _ type_rep += Pair : 'a type_rep * 'b type_rep -> ('a * 'b) type_rep

module Typeable0_make(T: sig type t end) =
struct
  type _ type_rep += T : T.t type_rep
  type t = T.t
  let eqty : type b. b type_rep -> (t, b) eql option =
    function T -> Some Refl | _ -> None
  let type_rep () = T
end

implicit module Typeable_int = Typeable0_make(struct type t = int end)
implicit module Typeable_bool = Typeable0_make(struct type t = bool end)
implicit module Typeable_float = Typeable0_make(struct type t = float end)
implicit module Typeable_string = Typeable0_make(struct type t = string end)

implicit module Typeable_pair{A: TYPEABLE} {B: TYPEABLE} =
struct
  type t = A.t * B.t
  let eqty : type c. c type_rep -> (A.t * B.t, c) eql option = function
      Pair (a, b) ->
      begin match A.eqty a, B.eqty b with
          Some Refl, Some Refl -> Some Refl
        | _ -> None
      end
    | _ -> None

  let type_rep () = Pair (A.type_rep (), B.type_rep ())
end

implicit module Typeable_list{A: TYPEABLE} =
struct
  type t = A.t list
  let eqty : type b. b type_rep -> (A.t list, b) eql option = function
    | List a ->
      begin match A.eqty a with
          Some Refl -> Some Refl
        | None -> None
      end
    | _ -> None
  let type_rep () = List (A.type_rep ())
end

implicit module Typeable_option{A: TYPEABLE} =
struct
  type t = A.t option
  let eqty : type b. b type_rep -> (A.t option, b) eql option = function
    | Option a ->
      begin match A.eqty a with
          Some Refl -> Some Refl
        | None -> None
      end
    | _ -> None
  let type_rep () = Option (A.type_rep ())
end

implicit module Data_int =
struct
  type t = int
  module Typeable = Typeable_int
  let gmapT _ x = x
  let gmapT_ _ x = x
  let gmapQ _ _ = []
  let gmapQ_ _ _ = .<[]>.
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) x = u#u x
  let gfoldl_ (g : _ genericFapp_) (u : _ genericFunit_) x = u#u x
  let constructor x = Syb_constructors.constructor (string_of_int x)
  let constructor_ x = .< Syb_constructors.constructor (string_of_int .~x) >.
end

implicit module Data_bool =
struct
  type t = bool
  module Typeable = Typeable_bool
  let gmapT _ x = x
  let gmapT_ _ x = x
  let gmapQ _ _ = []
  let gmapQ_ _ _ = .<[]>.
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) x = u#u x
  let gfoldl_ (g : _ genericFapp_) (u : _ genericFunit_) x = u#u x
  let constructor x = Syb_constructors.constructor (string_of_bool x)
  let constructor_ x = .< Syb_constructors.constructor (string_of_bool .~x) >.
end

implicit module Data_float =
struct
  type t = float
  module Typeable = Typeable_float
  let gmapT _ x = x
  let gmapT_ _ x = x
  let gmapQ _ _ = []
  let gmapQ_ _ _ = .<[]>.
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) x = u#u x
  let gfoldl_ (g : _ genericFapp_) (u : _ genericFunit_) x = u#u x
  let constructor x = Syb_constructors.constructor (string_of_float x)
  let constructor_ x = .< Syb_constructors.constructor (string_of_float .~x) >.
end

implicit module Data_string =
struct
  type t = string
  module Typeable = Typeable_string
  let gmapT _ x = x
  let gmapT_ _ x = x
  let gmapQ _ _ = []
  let gmapQ_ _ _ = .<[]>.
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) x = u#u x
  let gfoldl_ (g : _ genericFapp_) (u : _ genericFunit_) x = u#u x
  let constructor x = Syb_constructors.constructor (Printf.sprintf "%S" x)
  let constructor_ x = .< Syb_constructors.constructor (Printf.sprintf "%S" .~x) >.
end

implicit module Data_list {A: DATA} : DATA with type t = A.t list =
struct
  module rec R : DATA with type t = A.t list =
  struct
    type t = A.t list 
    module Typeable = Typeable_list(A.Typeable)
    let gmapT (f : genericT) (l : t) =
      match l with
        [] -> []
      | x :: xs -> f x :: f {R} xs

    let gmapT_ (f : genericT_) l =
      .< match .~l with 
          [] -> []
        | x :: xs -> .~(f .<x>.) :: .~(f {R} .<xs>.) >.

    let gmapQ (q : _ genericQ) (l : t) =
      match l with
        [] -> []
      | x :: xs -> [q x; q {R} xs]

    let gmapQ_  (q : _ genericQ_) l =
      .< match .~l with                   
        | [] -> []
        | x :: xs -> [.~(q .<x>.); .~(q {R} .<xs>.)] >.

    let gfoldl (g : _ genericFapp) (u : _ genericFunit) l =
      match l with
        [] -> u#u []
      | x :: xs -> g#g {R} (g#g (u#u (fun x xs -> x :: xs)) x) xs

    let gfoldl_ (g : 'c genericFapp_) (u : 'c genericFunit_) (l : t code) =
      .< match .~l with
          [] -> .~(u#u .<[]>.)
        | x :: xs -> .~(g#g {R} (g#g (u#u .<(fun x xs -> x :: xs)>.) .<x>.) .<xs>.) >.

    let constructor = function
        [] -> Syb_constructors.constructor "[]"
      | _::_ -> Syb_constructors.Cons

    let constructor_ c = .< match .~c with
       [] -> Syb_constructors.constructor "[]"
      | _::_ -> Syb_constructors.Cons >.
  end
  include R
end

implicit module Data_pair {A: DATA} {B: DATA} : DATA with type t = A.t * B.t =
struct
  type t = A.t * B.t
  module Typeable = Typeable_pair(A.Typeable)(B.Typeable)
  let gmapT (f : genericT) ((x, y) : t) = (f x, f y)
  let gmapT_ (f : genericT_) (p : (A.t * B.t) code) = 
    .< let (x, y) = .~p in (.~(f .<x>.), .~(f .<y>.)) >.
  let gmapQ (q : _ genericQ) ((x, y) : t) = [q x; q y]
  let gmapQ_ (q : _ genericQ_) (p : (A.t * B.t) code) =
    .< let (x, y) = .~p in [.~(q .<x>.); .~(q .<y>.)] >.
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) (x, y) =
    g#g {B} (g#g {A} (u#u (fun x y -> (x,y))) x) y
  let gfoldl_ (g : 'c genericFapp_) (u : 'c genericFunit_) (p : t code) =
     .< let (x, y) = .~p in .~(g#g {B} (g#g {A} (u#u .<fun x y -> (x,y)>.) .<x>.) .<y>.) >.
  let constructor _ = Syb_constructors.Tuple 2
  let constructor_ _ = .< Syb_constructors.Tuple 2 >.

end

implicit module Data_option {A: DATA} : DATA with type t = A.t option =
struct
  type t = A.t option
  module Typeable = Typeable_option(A.Typeable)
  let gmapT (f : genericT) (o : t) =
    match o with None -> None | Some x -> Some (f x)
  let gmapT_ (f : genericT_) (o : A.t option code) = 
    .< match .~o with None -> None | Some x -> Some .~(f .<x>.) >.
  let gmapQ (q : _ genericQ) (o : t) =
    match o with None -> [] | Some x -> [q x]
  let gmapQ_ (q : _ genericQ_) (o : A.t option code) =
    .< match .~o with None -> [] | Some x -> [.~(q .<x>.)] >.
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) = function
      None -> u#u None
    | Some x -> g#g {A} (u#u (fun x -> Some x)) x
  let gfoldl_ (g : _ genericFapp_) (u : _ genericFunit_) (o : t code) =
    .< match .~o with
        None -> .~(u#u .<None>.)
      | Some x -> .~(g#g {A} (u#u .<fun x -> Some x>.) .<x>.) >.
  let constructor = function
      None -> Syb_constructors.constructor "None"
    | Some _ -> Syb_constructors.constructor "Some"
  let constructor_ c = .< constructor .~c >.
end

implicit module Typeable_of_data{F: DATA} = F.Typeable
