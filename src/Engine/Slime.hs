module Engine.Slime
  ( module Engine.Slime
  , module Engine.Slime.Resolve
  ) where

import           Engine.Internal.Type
import           Engine.Slime.Resolve
import qualified Engine.Util.MaybeNat as MN

satSub :: Slime -> Slime -> Slime
satSub (Slime a) (Slime b) = Slime $ MN.satSub a b

