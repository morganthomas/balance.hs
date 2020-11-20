{-# LANGUAGE TypeFamilies #-}


module Balance.Element.Stack ( Stack (..), FromStack (..), stack, StackParams (..) ) where


import Balance.Element
import Balance.Geometry
import Balance.Element.Rectangular

import Control.Lens
import Control.Monad (forM_)
import Data.Proxy


newtype Stack e = Stack [e]


class FromStack e where
  fromStack :: Stack e -> e


stack :: FromStack e => [e] -> e
stack = fromStack . Stack


newtype StackParams e a = StackParams [Params e a]


instance Element e => Element (Stack e) where
  type Params (Stack e) = StackParams e
  type PenaltyConstraints (Stack e) a = (PenaltyConstraints e a, Num a)
  penalty (Stack es) = \(StackParams ps) -> sum $ zipWith penalty es ps
  guess _ (Stack es) = StackParams $ guess Proxy <$> es
  render (Stack es) (StackParams ps) surface = forM_ (zip es ps) $ \(e,p) -> render e p surface


instance RectangularElement e => RectangularElement (Stack e) where
  boundingBox _ = lens getBoundingBox setBoundingBox


pxy :: StackParams e a -> Proxy e
pxy _ = Proxy


getBoundingBox :: ( Num a, Ord a, RectangularElement e ) => StackParams e a -> Rectangle a
getBoundingBox p@(StackParams ps) = minimumBoundingRectangle $ (^. boundingBox (pxy p)) <$> ps


setBoundingBox :: ( Num a, Ord a, RectangularElement e ) => StackParams e a -> Rectangle a -> StackParams e a
setBoundingBox p@(StackParams ps) rect = StackParams
  $ (\q -> set (boundingBox (pxy p)) (minimumBoundingRectangle [rect, q ^. boundingBox (pxy p)]) q) <$> ps
