module Util where

import Types
import qualified Data.Map as M

-- NB. Overwrites prefer the first state!
-- O(n+m)
mergeBindings :: FievelState -> FievelState -> FievelState
mergeBindings (FievelState ts es) (FievelState ts' es') = 
  FievelState (ts `M.union` ts') (es `M.union` es')