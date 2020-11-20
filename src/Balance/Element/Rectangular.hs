{-# LANGUAGE RankNTypes #-}


module Balance.Element.Rectangular ( RectangularElement (..) ) where


import Balance.Element
import Balance.Geometry

import Control.Lens
import Data.Proxy


class Element e => RectangularElement e where
  boundingBox :: ( Num a, Ord a ) => Proxy e -> Lens' (Params e a) (Rectangle a)
