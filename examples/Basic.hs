{-# LANGUAGE ScopedTypeVariables #-}


module Main where


import Balance.Element
import Balance.Element.Std
import Balance.Surface.BlankCanvas

import Control.Monad (forM_)
import Graphics.Blank hiding (port)


port :: Int
port = 3000


options :: Options
options = Options port [] False "." [] False


main :: IO ()
main = do
  putStrLn $ "starting balance.hs basic example on port " <> show port
  let el :: StdEl Double = undefined
      approximations = optimize el
      maxIterations = 100
  blankCanvas options $ \ctx -> do
    putStrLn "opened a canvas"
    send ctx $ forM_ (take maxIterations approximations) $ \params -> render el params (BlankCanvasSurface ctx)
