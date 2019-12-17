open CoreCategory
include FreeSelective_intf


module Make(F:Functor.S1) : S1 with module F := F = struct

  type 'a t = 
    | Pure : 'a -> 'a t 
    | Select : ('a,'b) Either.t t * ('a -> 'b) F.t -> 'b t

  include Selective.MakeCustom1(struct

    type nonrec 'a t = 'a t

    let rec map_ : type a b.  a t -> f:(a -> b) -> b t = fun x ~f ->
      match x with 
      | Pure a -> Pure (f a)
      | Select(x,y) -> 
          Select
            ( map_ ~f:(Either.mapSecond ~f) x
            , F.map y ~f:(Fn.map ~f) 
            )

    let map = `Custom map_

    let replace = `Derived 

    let rec select : type a b. (a,b) Either.t t -> f:(a -> b) t -> b t = fun x ~f ->
      match f with 
      | Pure y -> 
          map_ x ~f:Either.(either ~first:y ~second:Fn.id)

      | Select(y,z) ->
        let f x = Either.mapSecond ~f:Either.second x
        and g y = fun a -> Either.bimap y ~first:(fun b -> b,a) ~second:((|>) a)
        and h z = Fn.uncurry z in 
        Select
          ( select (map_ x ~f) ~f:(map_ y ~f:g )
          , F.map z ~f:h
          )

    let pure x = Pure x

    let apply : type a b. a t -> f:(a -> b) t -> b t = fun x ~f ->
      select (map_ f ~f:Either.first) ~f:(map_ x ~f:((|>)))

    let liftA2 = `Derived 

    let applyFirst = `Derived 

    let applySecond = `Derived
  end)
end