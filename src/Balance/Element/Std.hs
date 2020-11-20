{-# LANGUAGE TypeFamilies #-}


module Balance.Element.Std
  ( StdEl
  , StdElF (..)
  , StdElParams
  ) where


import Balance.Element
import Balance.Element.Fill
import Balance.Element.Grid
import Balance.Element.HBox
import Balance.Element.Rectangular
import Balance.Element.Stack
import Balance.Element.VBox
import Balance.Geometry

import Data.Fix
import Numeric.AD (Mode)


data StdElF a e = FillEl (FillElement a)
--                | GridEl (Grid e a)
                | HBoxEl (HBox e a)
                | StackEl (Stack e)
                | VBoxEl (VBox e a)


type StdEl a = Fix (StdElF a)


data StdElParams e a = FillElParams (FillElementParams a)
--                     | GridElParams (GridParams e a)
                     | HBoxElParams (HBoxParams e a)
                     | StackElParams (StackParams e a)
                     | VBoxElParams (VBoxParams e a)


instance RectangularElement e => Element (StdElF a e) where
  type Params (StdElF a e) = StdElParams e
  type PenaltyConstraints (StdElF a e) b = ( b ~ a, Mode a, Ord a, PenaltyConstraints e a )
  penalty (FillEl e)  (FillElParams ps) = penalty e ps
--  penalty (GridEl e)  (GridElParams ps) = penalty e ps
  penalty (HBoxEl e)  (HBoxElParams ps) = penalty e ps
  penalty (StackEl e) (StackElParams ps) = penalty e ps
  penalty (VBoxEl e)  (VBoxElParams ps) = penalty e ps
