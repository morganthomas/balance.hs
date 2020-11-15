{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}


module Balance.Element.Fill ( FillElement (..), FillElementParams (..) ) where


import Balance.Element
import Balance.Element.Rectangular
import Balance.Geometry
import Balance.Penalty
import Balance.Surface

import Control.Lens
import Data.Colour
import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Tape, Reverse)


data FillElement = FillElement
  { fillElementPenalty :: forall a s. Reifies s Tape => FillElementParams (Reverse s a) -> Penalty (Reverse s a)
  , fillElementColor   :: Maybe (Colour Double) }


newtype FillElementParams a = FillElementParams { unFillElementParams :: (Rectangle a) }


instance Element FillElement where
  type Params FillElement = FillElementParams
  type PenaltyConstraints FillElement a = ( Real a, Fractional a )
  penalty = fillElementPenalty
  guess _ _ = FillElementParams (Rectangle (Coord 0 0) (Dimensions 0 0))
  render (FillElement _ (Just color)) (FillElementParams rect) surface =
    fillRectangle color rect surface
  render (FillElement _ Nothing) _ _ = return ()


instance RectangularElement FillElement where
  boundingBox _ = lens unFillElementParams (const FillElementParams)
