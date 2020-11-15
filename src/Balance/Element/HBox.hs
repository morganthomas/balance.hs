{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes   #-}


module Balance.Element.HBox ( HBox (..), hbox, HBoxParams (..) ) where


import Balance.Element
import Balance.Element.Rectangular
import Balance.Geometry
import Balance.Penalty

import Control.Lens hiding (children)
import Control.Monad (forM_)
import Data.Proxy


data HBox e = HBox
  { hboxPenalty  :: PenaltyFn
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
  type PenaltyConstraints (HBox e) a = (PenaltyConstraints e a, Real a, Fractional a, Ord a)
  penalty hb = \ps@(HBoxParams cps _) -> sum (zipWith penalty (hboxChildren hb) cps)
                                       + hboxPenalty hb (hboxError hb ps)
  guess pxy hb@(HBox _ es) =
    let subguesses = guess pxy <$> es
    in HBoxParams subguesses (minimumBoundingRectangle (view (boundingBox (childPxy hb)) <$> subguesses))
  render (HBox _ es) (HBoxParams ps _) surface = forM_ (zip es ps) $ \(e,p) -> render e p surface


hboxError :: RectangularElement e => Num a => HBox e -> HBoxParams e a -> Error a
hboxError _ (HBoxParams [] _) = 0
hboxError hb (HBoxParams [child] rect) =
  let childRect = view (boundingBox (childPxy hb)) child 
      x = coordX . rectangleCoord
      w = widthDim . rectangleDimensions in
   Error $
      abs (unLength . unXOffset $ x rect - x childRect)
    + abs (unLength $ unXOffset (farX (x rect) (w rect))
                    - unXOffset (farX (x childRect) (w childRect)))
hboxError hb (HBoxParams (firstChild:secondChild:children) rect) =
  let firstChildRect = view (boundingBox (childPxy hb)) firstChild
      x = unXOffset . coordX . rectangleCoord
      firstError = Error . unLength . abs $ x rect - x firstChildRect
      otherErrors = hboxError' hb firstChild secondChild children rect
  in firstError + otherErrors


hboxError' :: RectangularElement e => Num a => HBox e -> Params e a -> Params e a -> [Params e a] -> Rectangle a -> Error a
hboxError' hb prevChild lastChild [] rect =
  let prevChildRect = view (boundingBox (childPxy hb)) prevChild
      lastChildRect = view (boundingBox (childPxy hb)) lastChild
      far r = farX (coordX (rectangleCoord r)) (widthDim (rectangleDimensions r))
      innerEdgeError = Error . unLength . abs . unXOffset $ far rect - far lastChildRect
      outerEdgeError = Error . unLength . unWidth $ measureWidth (far prevChildRect) (coordX (rectangleCoord lastChildRect))
  in innerEdgeError + outerEdgeError
hboxError' hb prevChild child (nextChild:children) rect =
  let prevChildRect = view (boundingBox (childPxy hb)) prevChild
      childRect = view (boundingBox (childPxy hb)) child
      nextChildRect = view (boundingBox (childPxy hb)) nextChild
      far r = farX (coordX (rectangleCoord r)) (widthDim (rectangleDimensions r))
      x = coordX . rectangleCoord
      leftError = Error . unLength . unWidth $ measureWidth (far prevChildRect) (x childRect)
      rightError = Error . unLength . unWidth $ measureWidth (far childRect) (x nextChildRect)
      otherErrors = hboxError' hb child nextChild children rect
  in leftError + rightError + otherErrors


instance RectangularElement e => RectangularElement (HBox e) where
  boundingBox _ = lens (\(HBoxParams _ bb) -> bb)
                       (\(HBoxParams ps _) bb -> HBoxParams ps bb)
