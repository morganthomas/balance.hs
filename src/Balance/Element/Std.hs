{-# LANGUAGE TypeFamilies #-}


module Balance.Element.Std
  ( StdEl
  , StdElF (..)
  , StdElParams
  ) where


import Balance.Element
import Balance.Element.Fill
--import Balance.Element.Grid
import Balance.Element.HBox
import Balance.Element.Rectangular
import Balance.Element.Stack
import Balance.Element.VBox
import Balance.Geometry

import Control.Lens
import Data.Fix
import Data.Proxy
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

  penalty (FillEl e)  (FillElParams ps)  = penalty e ps
--  penalty (GridEl e)  (GridElParams ps) = penalty e ps
  penalty (HBoxEl e)  (HBoxElParams ps)  = penalty e ps
  penalty (StackEl e) (StackElParams ps) = penalty e ps
  penalty (VBoxEl e)  (VBoxElParams ps)  = penalty e ps
  penalty _           _                  = error "StdEl param type does not match element type"

  guess pxy (FillEl e)  = FillElParams  (guess pxy e)
  guess pxy (HBoxEl e)  = HBoxElParams  (guess pxy e)
  guess pxy (StackEl e) = StackElParams (guess pxy e)
  guess pxy (VBoxEl e)  = VBoxElParams  (guess pxy e)

  render (FillEl e)  (FillElParams p)  s = render e p s
  render (HBoxEl e)  (HBoxElParams p)  s = render e p s
  render (StackEl e) (StackElParams p) s = render e p s
  render (VBoxEl e)  (VBoxElParams p)  s = render e p s
  render _           _                 _ = error "StdEl param type does not match element type"


instance RectangularElement e => RectangularElement (StdElF a e) where
  boundingBox _ = lens getBoundingBox setBoundingBox


fillElPxy :: FillElementParams a -> Proxy (FillElement a)
fillElPxy _ = Proxy


hboxElPxy :: HBoxParams e a -> Proxy (HBox e a)
hboxElPxy _ = Proxy


vboxElPxy :: VBoxParams e a -> Proxy (VBox e a)
vboxElPxy _ = Proxy


stackElPxy :: StackParams e a -> Proxy (Stack e)
stackElPxy _ = Proxy


getBoundingBox :: ( Num a, Ord a, RectangularElement e ) => StdElParams e a -> Rectangle a
getBoundingBox (FillElParams  p) = p ^. boundingBox (fillElPxy p)
getBoundingBox (HBoxElParams  p) = p ^. boundingBox (hboxElPxy p)
getBoundingBox (StackElParams p) = p ^. boundingBox (stackElPxy p)
getBoundingBox (VBoxElParams p)  = p ^. boundingBox (vboxElPxy p)


setBoundingBox :: StdElParams e a -> Rectangle a -> StdElParams e a
setBoundingBox = undefined


--instance RectangularElement e => Element StdEl where
--  
