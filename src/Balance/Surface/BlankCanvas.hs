{-# LANGUAGE MultiParamTypeClasses #-}


module Balance.Surface.BlankCanvas ( BlankCanvasSurface (..) ) where


import Balance.Geometry
import Balance.Surface

import Graphics.Blank hiding (fillStyle)
import Graphics.Blank.Style (fillStyle)


newtype BlankCanvasSurface = BlankCanvasSurface DeviceContext


instance Surface Canvas BlankCanvasSurface where
  getSurfaceResolution (BlankCanvasSurface ctx) = return . Resolution $ 96.0 * devicePixelRatio ctx
  getSurfaceDimensions (BlankCanvasSurface ctx) = return $ Dimensions (Width (width ctx)) (Height (height ctx))
  fillRectangle color (Rectangle (Coord (XOffset (Length x)) (YOffset (Length y))) (Dimensions (Width (Length w)) (Height (Length h)))) _surface = do
    beginPath ()
    rect (x, y, w, h)
    fillStyle color
    fill ()
    closePath ()
