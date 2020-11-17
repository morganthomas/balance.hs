{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes       #-}


module Balance.Element.Grid ( Grid (..), FromGrid (..), grid, GridParams (..), edgeBelow, edgeAbove ) where


import Balance.Element
import Balance.Element.Rectangular
import Balance.Geometry
import Balance.Penalty

--import Control.Lens hiding (children)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
--import Data.Proxy
import Data.Reflection (Reifies)
import Numeric.AD (Mode)
import Numeric.AD.Internal.Reverse (Reverse, Tape)


data Grid e a = Grid
  { gridPenalty    :: PenaltyFn a
  , gridSize       :: Dimensions Int
  , gridChildren   :: Map (Coord Int) e }


class FromGrid e where
  fromGrid :: Grid (e a) a -> e a


grid :: FromGrid e => Mode a => Dimensions Int -> Map (Coord Int) (e a) -> e a
grid dims els = fromGrid $ Grid (quadraticPenalty prohibitiveQuadraticPenalty) dims els


data GridParams e a = GridParams
  { gridChildrenParams :: Map (Coord Int) (Params e a)
  , gridColOffsets     :: Map (XOffset Int) (XOffset (Length a))
  , gridRowOffsets     :: Map (YOffset Int) (YOffset (Length a))
  , gridBoundingBox    :: Rectangle a }


--childPxy :: Grid e a -> Proxy e
--childPxy _ = Proxy


edgeBelow :: RectangularElement e
          => Num a
          => Grid e a
          -> Coord Int
          -> forall s. Reifies s Tape
          => Maybe (GridParams e (Reverse s a)
                 -> YOffset (Length (Reverse s a)))
edgeBelow g (Coord x y) =
  let xlim = XOffset . unWidth . widthDim $ gridSize g
      ylim = YOffset . unHeight . heightDim $ gridSize g
  in if x >= 0 && x < xlim && y >= 0 && y < ylim
     then
       if y+1 == ylim
       then Just $ \params -> farY (coordY (rectangleCoord (gridBoundingBox params)))
                                   (heightDim (rectangleDimensions (gridBoundingBox params)))
       else case M.lookup (Coord x (y+1)) (gridChildren g) of
              Just _ -> Just $ \params -> fromMaybe (error "Balance.Element.Grid.edgeBelow: no row offset")
                                        $ M.lookup (y+1) (gridRowOffsets params)
              Nothing -> edgeBelow g (Coord x (y+1))
     else Nothing


edgeAbove :: RectangularElement e
          => Num a
          => Grid e a
          -> Coord Int
          -> forall s. Reifies s Tape
          => Maybe (GridParams e (Reverse s a)
                 -> YOffset (Length (Reverse s a)))
edgeAbove g (Coord x y) =
  let xlim = XOffset . unWidth . widthDim $ gridSize g
      ylim = YOffset . unHeight . heightDim $ gridSize g
  in if x >= 0 && x < xlim && y >= 0 && y < ylim
     then
       if y == 1
       then Just $ \params -> coordY (rectangleCoord (gridBoundingBox params))
       else case M.lookup (Coord x (y-1)) (gridChildren g) of
              Just _ -> Just $ \params -> fromMaybe (error "Balance.Element.Grid.edgeAbove: no row offset")
                                          $ M.lookup y (gridRowOffsets params)
              Nothing -> edgeAbove g (Coord x (y-1))
     else Nothing
