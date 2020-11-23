{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}


module Balance.Element.HBox ( HBox (..), FromHBox (..), hbox, HBoxParams (..) ) where


import Balance.Element
import Balance.Element.Rectangular
import Balance.Geometry
import Balance.Penalty

import Control.Lens hiding (children)
import Control.Monad (forM_)
import Data.Proxy
import Data.Reflection (Reifies)
import Numeric.AD (Mode)
import Numeric.AD.Internal.Reverse (Reverse, Tape)


data HBox e a = HBox
  { hboxPenalty  :: PenaltyFn a
  , hboxChildren :: [e] }


class FromHBox e where
  fromHBox :: HBox (e a) a -> e a


hbox :: FromHBox e => Mode a => [e a] -> e a
hbox = fromHBox . HBox (quadraticPenalty prohibitiveQuadraticPenalty)


data HBoxParams e a = HBoxParams
  { hboxChildrenParams :: [Params e a]
  , hboxBoundingBox    :: Rectangle a }

deriving instance Functor (Params e) => Functor (HBoxParams e)


childPxy :: HBox e a -> Proxy e
childPxy _ = Proxy


instance RectangularElement e => Element (HBox e a) where
  type Params (HBox e a) = HBoxParams e
  type PenaltyConstraints (HBox e a) b = ( b ~ a, PenaltyConstraints e a, Mode a, Ord a )
  penalty hb ps@(HBoxParams cps _) = sum (zipWith penalty (hboxChildren hb) cps)
                                   + hboxPenalty hb (hboxError hb ps)
  guess pxy hb@(HBox _ es) =
    let subguesses = guess pxy <$> es
    in HBoxParams subguesses (minimumBoundingRectangle (view (boundingBox (childPxy hb)) <$> subguesses))
  render (HBox _ es) (HBoxParams ps _) surface = forM_ (zip es ps) $ \(e,p) -> render e p surface


instance RectangularElement e => RectangularElement (HBox e a) where
  boundingBox _ = lens (\(HBoxParams _ bb) -> bb)
                       (\(HBoxParams ps _) bb -> HBoxParams ps bb)


verticalError :: RectangularElement e
              => Num a
              => Ord a
              => HBox e a
              -> forall s. Reifies s Tape
              => Rectangle (Reverse s a)
              -> Params e (Reverse s a)
              -> Error (Reverse s a)
verticalError hb rect childParams =
  let childRect = view (boundingBox (childPxy hb)) childParams
      y = coordY . rectangleCoord
      far r = farY (y r) (heightDim (rectangleDimensions r)) in
    Error $ unLength (abs . unYOffset $ y rect - y childRect)
          + unLength (abs . unYOffset $ far rect - far childRect)


hboxError :: RectangularElement e
          => Num a 
          => Ord a
          => HBox e a 
          -> forall s. Reifies s Tape
          => HBoxParams e (Reverse s a)
          -> Error (Reverse s a)
hboxError _ (HBoxParams [] _) = 0
hboxError hb (HBoxParams [child] rect) =
  let childRect = view (boundingBox (childPxy hb)) child 
      x = coordX . rectangleCoord
      w = widthDim . rectangleDimensions in
      Error (abs . unLength . unXOffset $ x rect - x childRect)
    + Error (abs . unLength $ unXOffset (farX (x rect) (w rect))
                            - unXOffset (farX (x childRect) (w childRect)))
    + verticalError hb rect child
hboxError hb (HBoxParams (firstChild:secondChild:children) rect) =
  let firstChildRect = view (boundingBox (childPxy hb)) firstChild
      x = unXOffset . coordX . rectangleCoord
      firstError = (Error . unLength . abs $ x rect - x firstChildRect)
                 + verticalError hb rect firstChild
      otherErrors = hboxError' hb firstChild secondChild children rect
  in firstError + otherErrors


hboxError' :: RectangularElement e
           => Num a
           => Ord a
           => HBox e a
           -> forall s. Reifies s Tape
           => Params e (Reverse s a)
           -> Params e (Reverse s a)
           -> [Params e (Reverse s a)]
           -> Rectangle (Reverse s a)
           -> Error (Reverse s a)
hboxError' hb prevChild lastChild [] rect =
  let prevChildRect = view (boundingBox (childPxy hb)) prevChild
      lastChildRect = view (boundingBox (childPxy hb)) lastChild
      far r = farX (coordX (rectangleCoord r)) (widthDim (rectangleDimensions r))
      vertError = verticalError hb rect lastChild
      innerEdgeError = Error . unLength . abs . unXOffset $ far rect - far lastChildRect
      outerEdgeError = Error . unLength . unWidth $ measureWidth (far prevChildRect) (coordX (rectangleCoord lastChildRect))
  in vertError + innerEdgeError + outerEdgeError
hboxError' hb prevChild child (nextChild:children) rect =
  let prevChildRect = view (boundingBox (childPxy hb)) prevChild
      childRect = view (boundingBox (childPxy hb)) child
      nextChildRect = view (boundingBox (childPxy hb)) nextChild
      far r = farX (coordX (rectangleCoord r)) (widthDim (rectangleDimensions r))
      x = coordX . rectangleCoord
      vertError = verticalError hb rect child
      leftError = Error . unLength . unWidth $ measureWidth (far prevChildRect) (x childRect)
      rightError = Error . unLength . unWidth $ measureWidth (far childRect) (x nextChildRect)
      otherErrors = hboxError' hb child nextChild children rect
  in vertError + leftError + rightError + otherErrors
