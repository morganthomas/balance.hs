{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes       #-}


module Balance.Element.Grid ( Grid (..), grid, GridParams (..) ) where


import Balance.Element
import Balance.Element.Rectangular
import Balance.Geometry
import Balance.Penalty

import Control.Lens hiding (children)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import Data.Reflection (Reifies)
import Numeric.AD (Mode)
import Numeric.AD.Internal.Reverse (Reverse, Tape)


data Grid e a = Grid
  { gridPenalty    :: PenaltyFn a
  , gridSize       :: Dimensions Int
  , gridChildren   :: Map (Coord Int) e }


grid :: Mode a => Dimensions Int -> Map (Coord Int) e -> Grid e a
grid = Grid (quadraticPenalty prohibitiveQuadraticPenalty)


data GridParams e a = GridParams
  { gridChildrenParams :: Map (Coord Int) (Params e a)
  , gridColWidths      :: Map Int (Width (Length a))
  , gridRowHeights     :: Map Int (Height (Length a))
  , gridBoundingBox    :: Rectangle a }


childPxy :: Grid e a -> Proxy e
childPxy _ = Proxy


edgeBelow :: RectangularElement e => Num a
          => Grid e a
          -> Coord Int
          -> forall s. Reifies s Tape
          => Maybe (GridParams e (Reverse s a)
                  -> YOffset (Length (Reverse s a)))
edgeBelow g (Coord x y) =
  let xlim = XOffset . unWidth . widthDim $ gridSize g
      ylim = YOffset . unHeight . heightDim $ gridSize g
  in if x < xlim && y < ylim
     then
       if y+1 == ylim
       then Just $ \params -> farY (coordY (rectangleCoord (gridBoundingBox params)))
                                   (heightDim (rectangleDimensions (gridBoundingBox params)))
       else case M.lookup (Coord x (y+1)) (gridChildren g) of
              Just _ -> Just $ \params -> coordY . rectangleCoord . view (boundingBox (childPxy g))
                                $ M.findWithDefault (error "Balance.Element.Grid.edgeBelow: element without params")
                                  (Coord x (y+1)) (gridChildrenParams params)
              Nothing -> edgeBelow g (Coord x (y+1))
     else Nothing
