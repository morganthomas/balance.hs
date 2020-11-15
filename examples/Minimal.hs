{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-redundant-constraints #-}

module Main where


import Balance.Element
import Balance.Geometry
import Balance.Penalty
import Balance.Surface
import Balance.Surface.BlankCanvas

import Control.Monad (forM_)
import Data.Colour.SRGB
import Data.Text (Text)
import Graphics.Blank hiding (port)

default (Text)


data Basic = Basic


data BasicParams a = BasicParams a

instance Functor BasicParams where
  fmap f (BasicParams x) = BasicParams (f x)

instance Foldable BasicParams where
  foldr f x (BasicParams y) = f y x

instance Traversable BasicParams where
  traverse f (BasicParams x) = BasicParams <$> f x


instance Element Basic where
  type Params Basic = BasicParams
  type PenaltyConstraints Basic a = Fractional a
  penalty Basic = \(BasicParams x) -> Penalty x
  guess _ Basic = BasicParams 100.0
  render Basic (BasicParams x) surface = do
    dims <- getSurfaceDimensions surface
    fillRectangle (sRGB (x / 100.0) 0.0 0.0)
                  (Rectangle (Coord 0 0) dims)
                  surface


port :: Int
port = 3000


options :: Options
options = Options port [] False "." [] False


main :: IO ()
main = do
  putStrLn $ "starting balance.hs minimal example on port " <> show port
  let el = Basic
      approximations = optimize el
      maxIterations = 100
  blankCanvas options $ \ctx -> do
    putStrLn "opened a canvas"
    send ctx $ forM_ (take maxIterations approximations) $ \params -> do
      console_log "rendering"
      render el params (BlankCanvasSurface ctx)
      console_log "rendered"
      sync
