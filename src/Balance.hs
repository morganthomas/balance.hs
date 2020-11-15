{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}


module Balance where


import Data.Colour (Colour)
import Data.Proxy
import Data.Reflection (Reifies)
import GHC.Exts (Constraint)
import Numeric.AD (gradientDescent)
import Numeric.AD.Internal.Reverse (Tape, Reverse)


-- |Lengths are in dots, a surface dependent unit of measure representing the surface's resolution.
newtype Length = Length { unLength :: Double }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)


-- |Resolution is in dots per inch.
newtype Resolution = Resolution { unResolution :: Double }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)


newtype Width = Width { unWidth :: Length }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)


newtype Height = Height { unHeight :: Length }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)


data Dimensions = Dimensions { widthDim :: Width, heightDim :: Height }


newtype XOffset = XOffset { unXOffset :: Length }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)


newtype YOffset = YOffset { unYOffset :: Length }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)


-- |A coordinate is specified as a pair of lengths, the length to descend from the top and the
-- length to move from the left to get to the coordinate.
data Coord = Coord { coordX :: XOffset, coordY :: YOffset }


data Rectangle = Rectangle { rectangleCoord :: Coord, rectangleDimensions :: Dimensions }


-- |A rectangular surface which we can drawn on within the monad m.
class Monad m => Surface m s where
  -- |Obtain the resolution of this surface in dots per inch.
  getSurfaceResolution :: s -> m Resolution
  -- |Obtain the dimensions of this surface in dots.
  getSurfaceDimensions :: s -> m Dimensions
  -- |Fill the given rectangle completely with the given color.
  fillRectangle :: Colour Double -> Rectangle -> s -> m ()


class Element e where
  type Params e :: * -> *
  type BadnessConstraints e a :: Constraint
  badness :: BadnessConstraints e a => e -> (forall s. Reifies s Tape => Params e (Reverse s a) -> Reverse s a)
  guess :: BadnessConstraints e a => Proxy a -> e -> Params e a
  render :: Surface m s => e -> Params e Double -> s -> m ()


-- |Returns a lazy list of progressively more optimal params for the given element.
optimize :: Element e
         => Fractional a
         => Ord a
         => BadnessConstraints e a
         => Traversable (Params e)
         => e
         -> [Params e a]
optimize e = gradientDescent (badness e) (guess Proxy e)
