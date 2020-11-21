{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes       #-}


module Balance.Element.Grid ( Grid (..), FromGrid (..), grid, GridParams (..) ) where


import Balance.Element
import Balance.Element.Rectangular
import Balance.Geometry
import Balance.Penalty

import Control.Lens hiding (children)
import Control.Monad (forM_)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Proxy
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


instance RectangularElement e => Element (Grid e a) where
  type Params (Grid e a) = GridParams e
  type PenaltyConstraints (Grid e a) b = ( b ~ a, PenaltyConstraints e a, Mode a, Ord a, Fractional a )
  penalty g ps = sum (zipWith penalty (M.elems (gridChildren g)) (M.elems (gridChildrenParams ps)))
               + gridPenalty g (gridError g ps)
  guess pxy g =
    let subguesses = guess pxy <$> gridChildren g
        bb = minimumBoundingRectangle . M.elems $ view (boundingBox (childPxy g)) <$> subguesses
        numColOffsets = subtract 1 . unWidth  . widthDim  $ gridSize g
        numRowOffsets = subtract 1 . unHeight . heightDim $ gridSize g
        bbOrigin = rectangleCoord bb
        bbDim = rectangleDimensions bb
        colMin = coordX bbOrigin
        colMax = farX (coordX bbOrigin) (widthDim bbDim)
        colGuesses = M.fromList $ (\n -> (n, colMin + (colMax - colMin)
                                                    * (fromIntegral n / fromIntegral (numColOffsets - 1))))
                                . XOffset
                               <$> [0..numColOffsets-1]
        rowMin = coordY bbOrigin
        rowMax = farY (coordY bbOrigin) (heightDim bbDim)
        rowGuesses = M.fromList $ (\n -> (n, rowMin + (rowMax - rowMin)
                                                    * (fromIntegral n / fromIntegral (numRowOffsets - 1))))
                                . YOffset
                               <$> [0..numRowOffsets-1]
    in GridParams subguesses colGuesses rowGuesses bb
  render g ps surface = forM_ (zip (M.elems (gridChildren g)) (M.elems (gridChildrenParams ps)))
                         $ \(e,p) -> render e p surface


childPxy :: Grid e a -> Proxy e
childPxy _ = Proxy


gridError :: RectangularElement e
          => Num a
          => Ord a
          => Grid e a 
          -> forall s. Reifies s Tape
          => GridParams e (Reverse s a)
          -> Error (Reverse s a)
gridError g ps = fromMaybe (error "gridError failed") $ do
  fs <- sequence (childError g <$> M.keys (gridChildren g))
  return . sum $ ($ ps) <$> fs


childError :: RectangularElement e
           => Num a
           => Ord a
           => Grid e a
           -> Coord Int
           -> forall s. Reifies s Tape
           => Maybe (GridParams e (Reverse s a)
                  -> Error (Reverse s a))
childError g xy = undefined


edgeBelow :: RectangularElement e
          => Num a
          => Grid e a
          -> Coord Int
          -> forall s. Reifies s Tape
          => Maybe (GridParams e (Reverse s a)
                 -> YOffset (Length (Reverse s a)))
edgeBelow g (Coord x y) =
  let xlim = subtract 1 . XOffset . unWidth  . widthDim  $ gridSize g
      ylim = subtract 1 . YOffset . unHeight . heightDim $ gridSize g
  in if x >= 0 && x < xlim && y >= 0 && y < ylim
     then
       if y+1 == ylim
       then Just $ \params -> farY (coordY (rectangleCoord (gridBoundingBox params)))
                                   (heightDim (rectangleDimensions (gridBoundingBox params)))
       else case M.lookup (Coord x y) (gridChildren g) of
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
  let xlim = subtract 1 . XOffset . unWidth  . widthDim  $ gridSize g
      ylim = subtract 1 . YOffset . unHeight . heightDim $ gridSize g
  in if x >= 0 && x < xlim && y >= 0 && y < ylim
     then
       if y == 0
       then Just $ \params -> coordY (rectangleCoord (gridBoundingBox params))
       else case M.lookup (Coord x (y-1)) (gridChildren g) of
              Just _ -> Just $ \params -> fromMaybe (error "Balance.Element.Grid.edgeAbove: no row offset")
                                          $ M.lookup y (gridRowOffsets params)
              Nothing -> edgeAbove g (Coord x (y-1))
     else Nothing


edgeLeft :: RectangularElement e
         => Num a
         => Grid e a
         -> Coord Int
         -> forall s. Reifies s Tape
         => Maybe (GridParams e (Reverse s a)
                -> XOffset (Length (Reverse s a)))
edgeLeft g (Coord x y) =
  let xlim = subtract 1 . XOffset . unWidth  . widthDim  $ gridSize g
      ylim = subtract 1 . YOffset . unHeight . heightDim $ gridSize g
  in if x >= 0 && x < xlim && y >= 0 && y < ylim
     then
       if x == 0
       then Just $ \params -> coordX (rectangleCoord (gridBoundingBox params))
       else case M.lookup (Coord (x-1) y) (gridChildren g) of
              Just _ -> Just $ \params -> fromMaybe (error "Balance.Element.Grid.edgeLeft: no column offset")
                                          $ M.lookup (x-1) (gridColOffsets params)
              Nothing -> edgeLeft g (Coord (x-1) y)
       else Nothing


edgeRight :: RectangularElement e
          => Num a
          => Grid e a
          -> Coord Int
          -> forall s. Reifies s Tape
          => Maybe (GridParams e (Reverse s a)
                 -> XOffset (Length (Reverse s a)))
edgeRight g (Coord x y) =
  let xlim = subtract 1 . XOffset . unWidth  . widthDim  $ gridSize g
      ylim = subtract 1 . YOffset . unHeight . heightDim $ gridSize g
  in if x >= 0 && x < xlim && y >= 0 && y < ylim
     then
       if x+1 == xlim
       then Just $ \params -> farX (coordX (rectangleCoord (gridBoundingBox params)))
                                   (widthDim (rectangleDimensions (gridBoundingBox params)))
       else case M.lookup (Coord x y) (gridChildren g) of
              Just _ -> Just $ \params -> fromMaybe (error "Balance.Element.Grid.edgeRight: no column offset")
                                           $ M.lookup (x+1) (gridColOffsets params)
              Nothing -> edgeRight g (Coord (x+1) y)
     else Nothing
