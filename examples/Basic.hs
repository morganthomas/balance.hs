{-# LANGUAGE ScopedTypeVariables #-}


module Main where


import Balance.Element
import Balance.Element.Fill
import Balance.Element.Layers
import Balance.Element.Std
import Balance.Surface
import Balance.Surface.BlankCanvas

import Control.Monad (forM_)
import Data.Colour.SRGB
import Data.Fix
import Data.Text (pack)
import Graphics.Blank hiding (port)


port :: Int
port = 3000


options :: Options
options = Options port [] False "." [] False


main :: IO ()
main = do
  putStrLn $ "starting balance.hs basic example on port " <> show port
  blankCanvas options $ \ctx -> do
    putStrLn "opened a canvas"
    send ctx $ do
      let surface = BlankCanvasSurface ctx
      dims <- getSurfaceDimensions surface
      let rigidEl = Fix . FillEl . fillVeryRigid dims $ Nothing
          blackEl = Fix . FillEl $ fillFlex (Just (sRGB 0 0 0))
          el :: StdEl Double = Fix . LayersEl $ layers [rigidEl, blackEl]
          approximations = optimize el
          maxIterations = 1000
      forM_ (take maxIterations approximations) $ \params -> do
        console_log (pack (show params))
        render el params surface
