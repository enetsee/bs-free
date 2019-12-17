open CoreCategory

module type S1 = sig 
  module F:  Functor.S1 
  include Selective.S1 
end

module type FreeSelective = sig 
  module type S1 = S1 
  module Make(F:Functor.S1) : S1 with module F := F
end