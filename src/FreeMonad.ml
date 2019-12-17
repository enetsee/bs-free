open CoreCategory
include FreeMonad_intf

module Make(F : Functor.S1) : S1 with module F := F = struct
  type 'a t = 
    | Pure of 'a 
    | Eff of 'a t F.t 

  let effect x = Eff x

  let rec run t ~pure ~eff = 
    match t with 
    | Pure x -> pure x 
    | Eff e -> 
      eff @@ F.map ~f:(run ~pure ~eff) e

  let liftF e = 
    effect @@ F.map ~f:(fun x -> Pure x) e 


  include Monad.MakeCustom1(struct
    type nonrec 'a t = 'a t 
    
    let rec map_ t ~f = 
      match t with 
      | Pure x -> Pure (f x)
      | Eff e -> Eff(F.map ~f:(map_ ~f) e)

    let map = `Custom map_ 

    let replace = `Derived 

    let pure x = Pure x 

    let rec bind t ~f = 
      match t with 
      | Pure x -> f x 
      | Eff e -> 
        Eff ( F.map ~f:(bind ~f) e )

    let join = `Derived

    let rec apply_ t ~f = 
      match f, t with 
      | Pure g , Pure x -> Pure (g x)
      | Pure f , Eff e -> 
        Eff(F.map ~f:(map_ ~f) e)
      | Eff e , _ -> 
        Eff(F.map ~f:(fun f -> apply_ t ~f) e)
    
    let apply = `Custom apply_

    let liftA2 = `Derived 

    let applyFirst = `Derived 

    let applySecond = `Derived 

    let select = `Derived 

  end)
end


module MakeChurch(F : Functor.S1) : S1 with module F := F = struct
  
  type 'a t = 
    { run: 'r. ('a -> 'r) -> ('r F.t -> 'r) -> 'r }

  let effect x = 
    { run = fun kp kf -> 
      kf @@ F.map x ~f:(fun {run} -> run kp kf)
    }

  let pure x = { run = fun kp _ -> kp x }

  let run {run} ~pure ~eff = run pure eff

  let liftF e = effect @@ F.map ~f:pure e

  include Monad.MakeCustom1(struct
    type nonrec 'a t = 'a t 

    let pure  = pure 

    let bind {run} ~f = 
      { run = fun kp kf -> 
          run (fun x -> (f x).run kp kf) kf
      }
    
    let map_ {run} ~f = 
      { run = fun kp kf -> run (fun x -> kp @@ f x) kf }

    let map = `Custom map_

    let replace = `Derived 

    let apply_ t ~f =
      { run = fun kp kf -> 
        f.run (fun f -> t.run (fun x -> kp @@ f x) kf) kf 
      }

    let apply = `Custom apply_ 

    let liftA2 = `Derived 

    let applyFirst = `Derived 

    let applySecond = `Derived 

    let select = `Derived 

    let join_ mmx = 
      { run = fun kp kf -> mmx.run (fun mx -> mx.run kp kf) kf }

    let join = `Custom join_

  end)
end