open CoreCategory

module type S1  = sig 
  module F : Functor.S1 
  include Monad.S1
  val liftF : 'a F.t -> 'a t
  val effect : 'a t F.t -> 'a t 
  val run : 'a t -> pure:('a -> 'r) -> eff:('r F.t -> 'r) -> 'r 
end

module type FreeMonad = sig 
  module type S1 = S1 
  module Make(F : Functor.S1) : S1 with module F := F 
  module MakeChurch(F : Functor.S1) : S1 with module F := F
end