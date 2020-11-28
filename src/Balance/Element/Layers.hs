{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}


module Balance.Element.Layers ( Layers (..), layers, LayersParams (..) ) where


import Balance.Element
import Balance.Element.Rectangular
import Balance.Geometry
import Balance.Penalty

import Control.Lens
import Control.Monad (forM_)
import Data.Reflection (Reifies)
import Data.Proxy
import Numeric.AD (Mode)
import Numeric.AD.Internal.Reverse (Reverse, Tape)


data Layers e a = Layers
  { layersPenalty :: PenaltyFn a
  , layersChildren :: [e] }


layers :: Mode a => [e] -> Layers e a
layers = Layers (quadraticPenalty prohibitiveQuadraticPenalty)


newtype LayersParams e a = LayersParams { layersChildrenParams :: [Params e a] }

deriving instance (         Eq   (Params e a) ) => Eq   (LayersParams e a)
deriving instance ( Show a, Show (Params e a) ) => Show (LayersParams e a)
deriving instance ( Read a, Read (Params e a) ) => Read (LayersParams e a)
deriving instance Functor     (Params e) => Functor     (LayersParams e)
deriving instance Foldable    (Params e) => Foldable    (LayersParams e)
deriving instance Traversable (Params e) => Traversable (LayersParams e)


instance RectangularElement e => Element (Layers e a) where
  type Params (Layers e a) = LayersParams e
  type PenaltyConstraints (Layers e a) b = ( b ~ a, PenaltyConstraints e a, Num a, Ord a )
  penalty (Layers f es) ps@(LayersParams cps) = sum (zipWith penalty es cps)
                                              + f (layersError Proxy ps)
  guess pxy (Layers _ es) = LayersParams $ guess pxy <$> es
  render (Layers _ es) (LayersParams ps) surface = forM_ (zip es ps) $ \(e,p) -> render e p surface


instance RectangularElement e => RectangularElement (Layers e a) where
  boundingBox _ = lens (\ps -> layersBoundingBox (childPxy ps) ps)
                       (\ps@(LayersParams cps) bb -> LayersParams
                         $ set (boundingBox (childPxy ps))
                               (minimumBoundingRectangle [ layersBoundingBox (childPxy ps) ps
                                                         , bb ])
                               <$> cps)


layersBoundingBox :: RectangularElement e
                  => Num a
                  => Ord a
                  => Proxy e
                  -> LayersParams e a
                  -> Rectangle a
layersBoundingBox pxy (LayersParams cps) = minimumBoundingRectangle (view (boundingBox pxy) <$> cps)


childPxy :: LayersParams e a -> Proxy e
childPxy _ = Proxy


layersError :: RectangularElement e
            => Num a
            => Ord a
            => Proxy e
            -> forall s. Reifies s Tape
            => LayersParams e (Reverse s a)
            -> Error (Reverse s a)
layersError pxy ps@(LayersParams cps) = sum $ childError pxy (layersBoundingBox pxy ps) <$> cps


childError :: RectangularElement e
           => Num a
           => Ord a
           => Proxy e
           -> forall s. Reifies s Tape
           => Rectangle (Reverse s a)
           -> Params e (Reverse s a)
           -> Error (Reverse s a)
childError pxy bb ps =
  let childBb = ps ^. boundingBox pxy
  in Error . unLength $ rectangleDistance bb childBb


rectangleDistance :: Num a
                  => Rectangle a
                  -> Rectangle a
                  -> Length a
rectangleDistance a b =
  let ax = coordX (rectangleCoord a)
      ay = coordY (rectangleCoord a)
      aw = widthDim (rectangleDimensions a)
      ah = heightDim (rectangleDimensions a)
      ax' = farX ax aw
      ay' = farY ay ah
      bx = coordX (rectangleCoord b)
      by = coordY (rectangleCoord b)
      bw = widthDim (rectangleDimensions b)
      bh = heightDim (rectangleDimensions b)
      bx' = farX bx bw
      by' = farY by bh
  in unXOffset (abs (ax - bx))
   + unYOffset (abs (ay - by))
   + unXOffset (abs (ax' - bx'))
   + unYOffset (abs (ay' - by'))
