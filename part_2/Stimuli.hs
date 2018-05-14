module Stimuli where
import CLaSH.Prelude
import Types

-- Stimuli
instrs = [Push 3, Push 5, PopPush 6, Pop, Pop]

-- Expected results
expResults = (0 :> 0 :> 5 :> 6 :> 3 :> Nil)
