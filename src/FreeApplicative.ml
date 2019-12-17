open CoreCategory
include FreeApplicative_intf

module Make(F: Functor.S1) : S1 with module F := F = struct

  type 'a t =
    | Pure : 'a -> 'a t
    | Ap : ('a -> 'b) t * ('a F.t)  -> 'b t

  let liftAp x = Ap(Pure(fun x -> x), x)

  include Applicative.MakeCustom1(struct
    
    type nonrec 'a t = 'a t

    let rec map_ : 'a 'b. 'a t -> f:('a -> 'b) -> 'b t = fun t ~f ->
      match t with 
      | Pure a -> Pure (f a)
      | Ap(ga,v) -> Ap(map_ ~f:(fun g x -> f (g x)) ga, v)

    let map = `Custom map_

    let replace = `Derived 

    let pure x = Pure x

    let rec apply :  'a 'b. 'a t -> f:('a -> 'b) t -> 'b t = fun t ~f ->
      match f with 
      | Pure g -> map_ ~f:g t
      | Ap(ga,v) -> Ap(apply  ~f:(map_ ~f:Fn.flip ga) t , v)

    let liftA2 = `Derived 

    let applyFirst = `Derived 

    let applySecond = `Derived

  end)  
end

module MakeOJ(F: Functor.S1) : S1 with module F := F = struct

    type 'a t = 
      | Pure : 'a -> 'a t
      | Ap : (('a -> 'b) t * 'a F.t) -> 'b t

    let liftAp x = Ap(Pure(fun x -> x), x)

    include Applicative.MakeCustom1(struct      
      type nonrec 'a t = 'a t
      
      let rec map_ : 'a 'b. 'a t -> f:('a -> 'b) -> 'b t = fun t ~f -> 
        match t with 
        | Pure a -> Pure (f a)
        | Ap(tx,ay) -> Ap(map_ ~f:(fun g x -> f (g x)) tx, ay)

      let map = `Custom map_ 

      let replace = `Derived 
      
      let pure x = Pure x 
    
      let rec apply : 'a 'b. 'a t -> f:('a -> 'b) t -> 'b t = fun t ~f ->
        match t with 
        | Pure y -> map_ ~f:(fun f -> f y) f
        | Ap(ty,az) -> Ap (apply ~f:(map_ ~f:Fn.compose f) ty , az)
    

      let liftA2 = `Derived 

      let applyFirst = `Derived 

      let applySecond = `Derived 

    end)
                 
end

(* Dave Menendez's free applicative: https://www.eyrie.org/~zednenem/2013/05/27/freeapp
module MakeFast_Base (X : Functor.Basic) = struct
    [@@@ocaml.warning "-37"]

    type 'a aseq =
      | ANil : unit aseq
      | ACons : ('a X.t * 'u aseq) -> ('a * 'u) aseq

    type ('y, 'z) fn = { apply : 'x. ('x -> 'y) -> 'x aseq -> 'z }

    let applyFn { apply } f s = apply f s

    type 'a t =
      { unap : 'u 'y 'z. ('y, 'z) fn -> ('u -> 'a -> 'y) -> 'u aseq -> 'z }

    let unAp { unap } fn k s = unap fn k s

    let pure a = { unap = (fun k f -> applyFn k (fun b -> f b a)) }

    let apply x ~f =
      { unap =
          (fun k e ->
            unAp
              x
              { apply = (fun h s -> unAp f k h s) }
              (fun s a g -> e s (g a)) )
      }


    let map x ~f = { unap = (fun k g -> unAp x k (fun s a -> g s (f a))) }

    include Make (struct
      type nonrec 'a t = 'a t

      let map = map

      let map_const = `Define_using_map

      let apply = apply

      let pure = pure
    end)

    let liftAp a =
      { unap = (fun k f s -> applyFn k (fun (a', s') -> f s' a') (ACons (a, s)))
      }
  end

  module MakeFast (X : Functor.Basic) : FreeS with type 'a f := 'a X.t = struct
    include MakeFast_Base (X)
  end

  module MakeFast_Applicative (X : Applicative_intf.Basic) :
    FreeS_Applicative with type 'a f := 'a X.t = struct
    include MakeFast_Base (X)

    let tuple2 x y = (x, y)

    (** Interprets the sequence of effects using the semantics for
        `pure` and `<*>` given by the Applicative instance for 'X.t'.
    *)
    let rec reduceASeq : type u. u aseq -> u X.t =
     fun s ->
      match s with
      | ANil ->
          X.pure ()
      | ACons (next, rest) ->
          X.(
            let f = map next ~f:tuple2 in
            apply (reduceASeq rest) ~f)


    let retractAp x : 'a X.t =
      unAp
        x
        { apply = (fun f s -> X.map ~f (reduceASeq s)) }
        (fun _ x -> x)
        ANil
  end *)