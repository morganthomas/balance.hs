{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes   #-}


module Balance.Element.HBox ( HBox (..), hbox, HBoxParams (..) ) where


import Balance.Element
import Balance.Element.Rectangular
import Balance.Geometry
import Balance.Penalty

import Control.Lens
import Control.Monad (forM_)
import Data.Proxy


data HBox e = HBox
  { hboxPenalty  :: Penalty
  , hboxChildren :: [e] }


hbox :: [e] -> HBox e
hbox = HBox (quadraticPenalty prohibitiveQuadraticPenalty)


data HBoxParams e a = HBoxParams
  { hboxChildrenParams :: [Params e a]
  , hboxBoundingBox    :: Rectangle a }


childPxy :: HBox e -> Proxy e
childPxy _ = Proxy


instance RectangularElement e => Element (HBox e) where
  type Params (HBox e) = HBoxParams e
  type PenaltyConstraints (HBox e) a = (PenaltyConstraints e a, Num a, Ord a)
  penalty (HBox _penalty es) = \(HBoxParams ps _) -> sum $ zipWith penalty es ps -- TODO: constraint of horizontal alignment within bounding box
  guess pxy hb@(HBox _ es) =
    let subguesses = guess pxy <$> es
    in HBoxParams subguesses (minimumBoundingRectangle (view (boundingBox (childPxy hb)) <$> subguesses))
  render (HBox _ es) (HBoxParams ps _) surface = forM_ (zip es ps) $ \(e,p) -> render e p surface


instance RectangularElement e => RectangularElement (HBox e) where
  boundingBox _ = lens (\(HBoxParams _ bb) -> bb)
                       (\(HBoxParams ps _) bb -> HBoxParams ps bb)
