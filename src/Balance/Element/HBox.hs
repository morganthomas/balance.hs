{-# LANGUAGE TypeFamilies #-}


module Balance.Element.HBox ( HBox (..), HBoxParams (..) ) where


import Balance.Element
import Balance.Element.Rectangular
import Balance.Geometry

import Control.Lens
import Control.Monad (forM_)
import Data.Proxy


newtype HBox e = HBox [e]


data HBoxParams e a = HBoxParams
  { hboxChildrenParams :: [Params e a]
  , hboxBoundingBox    :: Rectangle a }


childPxy :: HBox e -> Proxy e
childPxy _ = Proxy


instance RectangularElement e => Element (HBox e) where
  type Params (HBox e) = HBoxParams e
  type BadnessConstraints (HBox e) a = (BadnessConstraints e a, Num a, Ord a)
  badness (HBox es) = \(HBoxParams ps _) -> sum $ zipWith badness es ps -- TODO: constraint of horizontal alignment within bounding box
  guess pxy hb@(HBox es) =
    let subguesses = guess pxy <$> es
    in HBoxParams subguesses (minimumBoundingRectangle (view (boundingBox (childPxy hb)) <$> subguesses))
  render (HBox es) (HBoxParams ps _) surface = forM_ (zip es ps) $ \(e,p) -> render e p surface


instance RectangularElement e => RectangularElement (HBox e) where
  boundingBox _ = lens (\(HBoxParams _ bb) -> bb)
                       (\(HBoxParams ps _) bb -> HBoxParams ps bb)
