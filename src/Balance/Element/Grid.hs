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
