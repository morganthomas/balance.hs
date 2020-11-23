{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}


module Balance.Element.Fill
  ( Fill (..)
  , FillParams (..)
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


data Fill a = Fill
  { fillPenalty :: forall s. Reifies s Tape
                => FillParams (Reverse s a)
                -> Penalty (Reverse s a)
  , fillColor   :: Maybe (Colour Double) }


newtype FillParams a = FillParams { unFillParams :: Rectangle a }
  deriving (Functor, Foldable, Traversable)


instance Element (Fill a) where
  type Params (Fill a) = FillParams
  type PenaltyConstraints (Fill a) b = ( b ~ a, Mode a )
  penalty = fillPenalty
  guess _ _ = FillParams (Rectangle (Coord 0 0) (Dimensions 0 0))
  render (Fill _ (Just color)) (FillParams rect) surface =
    fillRectangle color rect surface
  render (Fill _ Nothing) _ _ = return ()


instance RectangularElement (Fill a) where
  boundingBox _ = lens unFillParams (const FillParams)


horizontallyRigidError :: ( Mode a, Scalar a ~ Double, Reifies s Tape )
                       => Width (Length Double)
                       -> FillParams (Reverse s a)
                       -> Error (Reverse s a)
horizontallyRigidError ideal (FillParams rect) =
  Error . abs . unLength . unWidth $ (fmap (auto . auto) <$> ideal) - widthDim (rectangleDimensions rect)


fillHorizontallyRigid :: ( Mode a, Scalar a ~ Double )
                      => PenaltyFn a
                      -> Width (Length Double)
                      -> Maybe (Colour Double)
                      -> Fill a
fillHorizontallyRigid pen ideal = Fill (pen . horizontallyRigidError ideal)


fillHorizontallyVeryRigid :: ( Mode a, Scalar a ~ Double )
                          => Width (Length Double)
                          -> Maybe (Colour Double)
                          -> Fill a
fillHorizontallyVeryRigid = fillHorizontallyRigid (quadraticPenalty prohibitiveQuadraticPenalty)


verticallyRigidError :: ( Mode a, Scalar a ~ Double, Reifies s Tape )
                     => Height (Length Double)
                     -> FillParams (Reverse s a)
                     -> Error (Reverse s a)
verticallyRigidError ideal (FillParams rect) =
  Error . abs . unLength . unHeight $ (Length . auto . auto . unLength <$> ideal)
                                    - heightDim (rectangleDimensions rect)


fillVerticallyRigid :: ( Mode a, Scalar a ~ Double )
                    => PenaltyFn a
                    -> Height (Length Double)
                    -> Maybe (Colour Double)
                    -> Fill a
fillVerticallyRigid pen ideal = Fill (pen . verticallyRigidError ideal)


fillVerticallyVeryRigid :: ( Mode a, Scalar a ~ Double )
                        => Height (Length Double)
                        -> Maybe (Colour Double)
                        -> Fill a
fillVerticallyVeryRigid = fillVerticallyRigid (quadraticPenalty prohibitiveQuadraticPenalty)


rigidError :: ( Mode a, Scalar a ~ Double, Reifies s Tape )
           => Dimensions (Length Double)
           -> FillParams (Reverse s a)
           -> Error (Reverse s a)
rigidError ideal (FillParams rect) =
  Error $ abs (unLength . unWidth  $ (Length . auto . auto . unLength <$> widthDim  ideal)
                                   - widthDim  (rectangleDimensions rect))
        + abs (unLength . unHeight $ (Length . auto . auto . unLength <$> heightDim ideal)
                                   - heightDim (rectangleDimensions rect))


fillRigid :: ( Mode a, Scalar a ~ Double )
          => PenaltyFn a
          -> Dimensions (Length Double)
          -> Maybe (Colour Double)
          -> Fill a
fillRigid pen ideal = Fill (pen . rigidError ideal)


fillVeryRigid :: ( Mode a, Scalar a ~ Double )
              => Dimensions (Length Double)
              -> Maybe (Colour Double)
              -> Fill a
fillVeryRigid = fillRigid (quadraticPenalty prohibitiveQuadraticPenalty)


fillFlex :: Num a
         => Maybe (Colour Double)
         -> Fill a
fillFlex = Fill (const 0)
