{-# LANGUAGE TypeFamilies #-}

module Balance.Element.Stack where


import Balance

import Control.Monad (forM_)
import Data.Proxy


data Stack e = Stack [e]


data StackParams e a = StackParams [Params e a]


instance Element e => Element (Stack e) where
    type Params (Stack e) = StackParams e
    type BadnessConstraints (Stack e) a = (BadnessConstraints e a, Num a)
    badness (Stack es) = \(StackParams ps) -> sum $ zipWith badness es ps
    guess _ (Stack es) = StackParams $ guess Proxy <$> es
    render (Stack es) (StackParams ps) surface = forM_ (zip es ps) $ \(e,p) -> render e p surface