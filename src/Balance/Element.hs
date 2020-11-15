{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}


module Balance.Element ( Element (..), optimize ) where


import Balance.Surface

import Data.Proxy
import Data.Reflection (Reifies)
import GHC.Exts (Constraint)
import Numeric.AD (gradientDescent)
import Numeric.AD.Internal.Reverse (Tape, Reverse)


class Element e where
  type Params e :: * -> *
  type PenaltyConstraints e a :: Constraint
  penalty :: PenaltyConstraints e a => e -> (forall s. Reifies s Tape => Params e (Reverse s a) -> Reverse s a)
  guess :: PenaltyConstraints e a => Proxy a -> e -> Params e a
  render :: Surface m s => e -> Params e Double -> s -> m ()


-- |Returns a lazy list of progressively more optimal params for the given element.
optimize :: Element e
         => Fractional a
         => Ord a
         => PenaltyConstraints e a
         => Traversable (Params e)
         => e
         -> [Params e a]
optimize e = gradientDescent (penalty e) (guess Proxy e)
