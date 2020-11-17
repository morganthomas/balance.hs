{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}


module Balance.Element.Fill
  ( FillElement (..)
  , FillElementParams (..)
  , fillHorizontallyRigid
  , fillHorizontallyVeryRigid
  , fillVerticallyRigid
  , fillVerticallyVeryRigid
  , fillRigid
  , fillVeryRigid
  , fillFlex
  ) where


import Balance.Element
import Balance.Element.Rectangular
import Balance.Geometry
import Balance.Penalty
import Balance.Surface

import Control.Lens
import Data.Colour
import Data.Reflection (Reifies)
import Numeric.AD (Scalar, Mode, auto)
import Numeric.AD.Internal.Reverse (Tape, Reverse)


data FillElement a = FillElement
  { fillElementPenalty :: forall s. Reifies s Tape
                       => FillElementParams (Reverse s a) -> Penalty (Reverse s a)
  , fillElementColor   :: Maybe (Colour Double) }


newtype FillElementParams a = FillElementParams { unFillElementParams :: (Rectangle a) }


instance Element (FillElement a) where
  type Params (FillElement a) = FillElementParams
  type PenaltyConstraints (FillElement a) b = ( b ~ a, Mode a )
  penalty = fillElementPenalty
  guess _ _ = FillElementParams (Rectangle (Coord 0 0) (Dimensions 0 0))
  render (FillElement _ (Just color)) (FillElementParams rect) surface =
    fillRectangle color rect surface
  render (FillElement _ Nothing) _ _ = return ()


instance RectangularElement (FillElement a) where
  boundingBox _ = lens unFillElementParams (const FillElementParams)


horizontallyRigidError :: ( Mode a, Scalar a ~ Double, Reifies s Tape )
                     => Width Double -> FillElementParams (Reverse s a) -> Error (Reverse s a)
horizontallyRigidError ideal (FillElementParams rect) =
  Error . abs . unLength . unWidth $ (auto . auto <$> ideal) - widthDim (rectangleDimensions rect)


fillHorizontallyRigid :: ( Mode a, Scalar a ~ Double )
                      => PenaltyFn a -> Width Double -> Maybe (Colour Double) -> FillElement a
fillHorizontallyRigid pen ideal = FillElement (pen . horizontallyRigidError ideal)


fillHorizontallyVeryRigid :: ( Mode a, Scalar a ~ Double )
                          => Width Double -> Maybe (Colour Double) -> FillElement a
fillHorizontallyVeryRigid = fillHorizontallyRigid (quadraticPenalty prohibitiveQuadraticPenalty)


verticallyRigidError :: ( Mode a, Scalar a ~ Double, Reifies s Tape )
                     => Height Double -> FillElementParams (Reverse s a) -> Error (Reverse s a)
verticallyRigidError ideal (FillElementParams rect) =
  Error . abs . unLength . unHeight $ (auto . auto <$> ideal) - heightDim (rectangleDimensions rect)


fillVerticallyRigid :: ( Mode a, Scalar a ~ Double )
                    => PenaltyFn a -> Height Double -> Maybe (Colour Double) -> FillElement a
fillVerticallyRigid pen ideal = FillElement (pen . verticallyRigidError ideal)


fillVerticallyVeryRigid :: ( Mode a, Scalar a ~ Double )
                        => Height Double -> Maybe (Colour Double) -> FillElement a
fillVerticallyVeryRigid = fillVerticallyRigid (quadraticPenalty prohibitiveQuadraticPenalty)


rigidError :: ( Mode a, Scalar a ~ Double, Reifies s Tape )
           => Dimensions Double -> FillElementParams (Reverse s a) -> Error (Reverse s a)
rigidError ideal (FillElementParams rect) =
  Error $ abs (unLength . unWidth  $ (auto . auto <$> widthDim  ideal)
                                   - widthDim  (rectangleDimensions rect))
        + abs (unLength . unHeight $ (auto . auto <$> heightDim ideal)
                                   - heightDim (rectangleDimensions rect))


fillRigid :: ( Mode a, Scalar a ~ Double )
          => PenaltyFn a -> Dimensions Double -> Maybe (Colour Double) -> FillElement a
fillRigid pen ideal = FillElement (pen . rigidError ideal)


fillVeryRigid :: ( Mode a, Scalar a ~ Double )
              => Dimensions Double -> Maybe (Colour Double) -> FillElement a
fillVeryRigid = fillRigid (quadraticPenalty prohibitiveQuadraticPenalty)


fillFlex :: Num a => Maybe (Colour Double) -> FillElement a
fillFlex = FillElement (const 0)
