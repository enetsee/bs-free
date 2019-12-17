open CoreCategory

module type S1 = sig
  module F : Functor.S1 
  include Applicative.S1
  val liftAp : 'a F.t -> 'a t  
end



module type FreeApplicative = sig 
  module type S1 = S1

  module Make(F: Functor.S1) : S1 with module F := F
  module MakeOJ(F: Functor.S1) : S1 with module F := F 

end