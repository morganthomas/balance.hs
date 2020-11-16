{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Balance.Geometry
  ( Length (..)
  , Resolution (..)
  , Width (..)
  , Height (..)
  , Dimensions (..)
  , XOffset (..)
  , YOffset (..)
  , farX
  , farY
  , measureWidth
  , measureHeight
  , Coord (..)
  , Rectangle (..)
  , measureRectangle
  , minimumBoundingRectangle
  ) where


import Data.List (foldl')


-- |Lengths are in dots, a surface dependent unit of measure representing the surface's resolution.
newtype Length a = Length { unLength :: a }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat, Functor)


-- |Resolution is in dots per inch.
newtype Resolution a = Resolution { unResolution :: a }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat, Functor)


newtype Width a = Width { unWidth :: Length a }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat, Functor)


newtype Height a = Height { unHeight :: Length a }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat, Functor)


data Dimensions a = Dimensions { widthDim :: Width a, heightDim :: Height a }


newtype XOffset a = XOffset { unXOffset :: Length a }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat, Functor)


newtype YOffset a = YOffset { unYOffset :: Length a }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat, Functor)


farX :: Num a => XOffset a -> Width a -> XOffset a
farX (XOffset x) (Width w) = XOffset (x + w)


farY :: Num a => YOffset a -> Height a -> YOffset a
farY (YOffset y) (Height h) = YOffset (y + h)


measureWidth :: Num a => XOffset a -> XOffset a -> Width a
measureWidth (XOffset x0) (XOffset x1) = Width (abs (x0 - x1))


measureHeight :: Num a => YOffset a -> YOffset a -> Height a
measureHeight (YOffset y0) (YOffset y1) = Height (abs (y0 - y1))


-- |A coordinate is specified as a pair of lengths, the length to descend from the top and the
-- length to move from the left to get to the coordinate.
data Coord a = Coord { coordX :: XOffset a, coordY :: YOffset a }


data Rectangle a = Rectangle { rectangleCoord :: Coord a, rectangleDimensions :: Dimensions a }


measureRectangle :: Num a => Coord a -> Coord a -> Dimensions a
measureRectangle (Coord x0 y0) (Coord x1 y1) = Dimensions (measureWidth x0 x1) (measureHeight y0 y1)


-- |Find the minimum bounding rectangle containing the given rectangles.
minimumBoundingRectangle :: Num a => Ord a => [Rectangle a] -> Rectangle a
minimumBoundingRectangle [] = Rectangle (Coord 0 0) (Dimensions 0 0)
minimumBoundingRectangle (rect:rects) = foldl' f rect rects
  where f (Rectangle (Coord x0 y0) (Dimensions w0 h0)) (Rectangle (Coord x1 y1) (Dimensions w1 h1)) =
          let c = Coord (min x0 x1) (min y0 y1)
              d = Coord (max (farX x0 w0) (farX x1 w1)) (max (farY y0 h0) (farY y1 h1))
          in Rectangle c (measureRectangle c d)
