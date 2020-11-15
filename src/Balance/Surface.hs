{-# LANGUAGE MultiParamTypeClasses #-}


module Balance.Surface ( Surface (..) ) where


import Balance.Geometry

import Data.Colour (Colour)


-- |A rectangular surface which we can draw on within the monad m.
class Monad m => Surface m s where
  -- |Obtain the resolution of this surface in dots per inch.
  getSurfaceResolution :: s -> m (Resolution Double)
  -- |Obtain the dimensions of this surface in dots.
  getSurfaceDimensions :: s -> m (Dimensions Double)
  -- |Fill the given rectangle completely with the given color.
  fillRectangle :: Colour Double -> Rectangle Double -> s -> m ()
