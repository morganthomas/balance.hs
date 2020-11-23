{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}


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

import Control.Lens
import Data.Fix
import Data.Proxy
import Numeric.AD (Mode)


data StdElF a e = FillEl (Fill a)
                | GridEl (Grid e a)
                | HBoxEl (HBox e a)
                | StackEl (Stack e)
                | VBoxEl (VBox e a)


type StdEl a = Fix (StdElF a)


data StdElParams e a = FillElParams (FillParams a)
                     | GridElParams (GridParams e a)
                     | HBoxElParams (HBoxParams e a)
                     | StackElParams (StackParams e a)
                     | VBoxElParams (VBoxParams e a)


instance RectangularElement e => Element (StdElF a e) where
  type Params (StdElF a e) = StdElParams e

  type PenaltyConstraints (StdElF a e) b =
    ( b ~ a
    , Mode a
    , Ord a
    , Num a
    , Fractional a
    , PenaltyConstraints e a )

  penalty (FillEl e)  (FillElParams ps)  = penalty e ps
  penalty (GridEl e)  (GridElParams ps) = penalty e ps
  penalty (HBoxEl e)  (HBoxElParams ps)  = penalty e ps
  penalty (StackEl e) (StackElParams ps) = penalty e ps
  penalty (VBoxEl e)  (VBoxElParams ps)  = penalty e ps
  penalty _           _                  = error "StdEl param type does not match element type"

  guess pxy (FillEl e)  = FillElParams  (guess pxy e)
  guess pxy (GridEl e)  = GridElParams  (guess pxy e)
  guess pxy (HBoxEl e)  = HBoxElParams  (guess pxy e)
  guess pxy (StackEl e) = StackElParams (guess pxy e)
  guess pxy (VBoxEl e)  = VBoxElParams  (guess pxy e)

  render (FillEl e)  (FillElParams p)  s = render e p s
  render (GridEl e)  (GridElParams p)  s = render e p s
  render (HBoxEl e)  (HBoxElParams p)  s = render e p s
  render (StackEl e) (StackElParams p) s = render e p s
  render (VBoxEl e)  (VBoxElParams p)  s = render e p s
  render _           _                 _ = error "StdEl param type does not match element type or there is no render case for a given element type"


instance RectangularElement e => RectangularElement (StdElF a e) where
  boundingBox _ = lens getBoundingBox setBoundingBox


fillElPxy :: FillParams a -> Proxy (Fill a)
fillElPxy _ = Proxy


gridElPxy :: GridParams e a -> Proxy (Grid e a)
gridElPxy _ = Proxy


hboxElPxy :: HBoxParams e a -> Proxy (HBox e a)
hboxElPxy _ = Proxy


vboxElPxy :: VBoxParams e a -> Proxy (VBox e a)
vboxElPxy _ = Proxy


stackElPxy :: StackParams e a -> Proxy (Stack e)
stackElPxy _ = Proxy


getBoundingBox :: ( Num a, Ord a, RectangularElement e )
               => StdElParams e a -> Rectangle a
getBoundingBox (FillElParams  p) = p ^. boundingBox (fillElPxy p)
getBoundingBox (GridElParams  p) = p ^. boundingBox (gridElPxy p)
getBoundingBox (HBoxElParams  p) = p ^. boundingBox (hboxElPxy p)
getBoundingBox (StackElParams p) = p ^. boundingBox (stackElPxy p)
getBoundingBox (VBoxElParams p)  = p ^. boundingBox (vboxElPxy p)


setBoundingBox :: ( Num a, Ord a, RectangularElement e )
               => StdElParams e a -> Rectangle a -> StdElParams e a
setBoundingBox (FillElParams  p) bb = FillElParams  $ set (boundingBox (fillElPxy  p)) bb p
setBoundingBox (GridElParams  p) bb = GridElParams  $ set (boundingBox (gridElPxy  p)) bb p
setBoundingBox (HBoxElParams  p) bb = HBoxElParams  $ set (boundingBox (hboxElPxy  p)) bb p
setBoundingBox (StackElParams p) bb = StackElParams $ set (boundingBox (stackElPxy p)) bb p
setBoundingBox (VBoxElParams  p) bb = VBoxElParams  $ set (boundingBox (vboxElPxy  p)) bb p


instance Element (StdEl a) where
  type Params (StdEl a) = StdElParams (StdEl a)
  type PenaltyConstraints (StdEl a) b = ( b ~ a, Num a, Fractional a, Mode a, Ord a )
  penalty (Fix e) = penalty e
  guess pxy (Fix e) = guess pxy e
  render (Fix e) = render e


instance RectangularElement (StdEl a) where
  boundingBox pxy = boundingBox (f pxy)
    where f :: Proxy (StdEl a) -> Proxy (StdElF a (StdEl a))
          f _ = Proxy
