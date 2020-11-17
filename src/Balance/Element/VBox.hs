{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes       #-}


module Balance.Element.VBox ( VBox (..), FromVBox (..), vbox, VBoxParams (..) ) where


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


data VBox e a = VBox
  { vboxPenalty  :: PenaltyFn a
  , vboxChildren :: [e] }


class FromVBox e where
  fromVBox :: VBox (e a) a -> e a


vbox :: FromVBox e => Mode a => [e a] -> e a
vbox = fromVBox . VBox (quadraticPenalty prohibitiveQuadraticPenalty)


data VBoxParams e a = VBoxParams
  { vboxChildrenParams :: [Params e a]
  , vboxBoundingBox    :: Rectangle a }


childPxy :: VBox e a -> Proxy e
childPxy _ = Proxy


instance RectangularElement e => Element (VBox e a) where
  type Params (VBox e a) = VBoxParams e
  type PenaltyConstraints (VBox e a) b = ( b ~ a, PenaltyConstraints e a, Mode a, Ord a )
  penalty vb = \ps@(VBoxParams cps _) -> sum (zipWith penalty (vboxChildren vb) cps)
                                       + vboxPenalty vb (vboxError vb ps)
  guess pxy vb@(VBox _ es) =
    let subguesses = guess pxy <$> es
    in VBoxParams subguesses (minimumBoundingRectangle (view (boundingBox (childPxy vb)) <$> subguesses))
  render (VBox _ es) (VBoxParams ps _) surface = forM_ (zip es ps) $ \(e,p) -> render e p surface


horizontalError :: RectangularElement e
                => Num a
                => VBox e a
                -> forall s. Reifies s Tape
                => Rectangle (Reverse s a)
                -> Params e (Reverse s a)
                -> Error (Reverse s a)
horizontalError vb rect childParams =
  let childRect = view (boundingBox (childPxy vb)) childParams
      x = coordX . rectangleCoord
      far r = farX (x r) (widthDim (rectangleDimensions r)) in
    Error $ unLength (abs . unXOffset $ x rect - x childRect)
          + unLength (abs . unXOffset $ far rect - far childRect)


vboxError :: RectangularElement e
          => Num a
          => VBox e a
          -> forall s. Reifies s Tape
          => VBoxParams e (Reverse s a)
          -> Error (Reverse s a)
vboxError _ (VBoxParams [] _) = 0
vboxError vb (VBoxParams [child] rect) =
  let childRect = view (boundingBox (childPxy vb)) child
      y = coordY . rectangleCoord
      h = heightDim . rectangleDimensions in
      Error (abs . unLength . unYOffset $ y rect - y childRect)
    + Error (abs . unLength $ unYOffset (farY (y rect) (h rect))
                            - unYOffset (farY (y childRect) (h childRect)))
    + horizontalError vb rect child
vboxError vb (VBoxParams (firstChild:secondChild:children) rect) =
  let firstChildRect = view (boundingBox (childPxy vb)) firstChild
      y = unYOffset . coordY . rectangleCoord
      firstError = (Error . unLength . abs $ y rect - y firstChildRect)
                 + horizontalError vb rect firstChild
      otherErrors = vboxError' vb firstChild secondChild children rect
  in firstError + otherErrors


vboxError' :: RectangularElement e
           => Num a
           => VBox e a
           -> forall s. Reifies s Tape
           => Params e (Reverse s a)
           -> Params e (Reverse s a)
           -> [Params e (Reverse s a)]
           -> Rectangle (Reverse s a)
           -> Error (Reverse s a)
vboxError' vb prevChild lastChild [] rect =
  let prevChildRect = view (boundingBox (childPxy vb)) prevChild
      lastChildRect = view (boundingBox (childPxy vb)) lastChild
      far r = farY (coordY (rectangleCoord r)) (heightDim (rectangleDimensions r))
      horizError = horizontalError vb rect lastChild
      innerEdgeError = Error . unLength . abs . unYOffset $ far rect - far lastChildRect
      outerEdgeError = Error . unLength . abs . unHeight $ measureHeight (far prevChildRect) (coordY (rectangleCoord lastChildRect))
  in horizError + innerEdgeError + outerEdgeError
vboxError' vb prevChild child (nextChild:children) rect =
  let prevChildRect = view (boundingBox (childPxy vb)) prevChild
      childRect = view (boundingBox (childPxy vb)) child
      nextChildRect = view (boundingBox (childPxy vb)) nextChild
      far r = farY (coordY (rectangleCoord r)) (heightDim (rectangleDimensions r))
      y = coordY . rectangleCoord
      horizError = horizontalError vb rect child
      upError = Error . unLength . unHeight $ measureHeight (far prevChildRect) (y childRect)
      downError = Error . unLength . unHeight $ measureHeight (far childRect) (y nextChildRect)
      otherErrors = vboxError' vb child nextChild children rect
  in horizError + upError + downError + otherErrors


instance RectangularElement e => RectangularElement (VBox e a) where
  boundingBox _ = lens (\(VBoxParams _ bb) -> bb)
                       (\(VBoxParams ps _) bb -> VBoxParams ps bb)
