{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}


module Balance.Penalty
  ( Error (..)
  , Penalty (..)
  , PenaltyFn
  , QuadraticPenalty (..)
  , quadraticPenalty
  , prohibitiveQuadraticPenalty
  ) where


import Data.Reflection (Reifies)
import Numeric.AD (Mode, auto)
import Numeric.AD.Internal.Reverse (Tape, Reverse)


newtype Error a = Error { unError :: a }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat, Functor)


newtype Penalty a = Penalty { unPenalty :: a }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat, Functor)


type PenaltyFn a = forall s. Reifies s Tape => Error (Reverse s a) -> Penalty (Reverse s a)


data QuadraticPenalty a = QuadraticPenalty
  { extraConstant        :: a
  , linearCoefficient    :: a
  , quadraticCoefficient :: a }


quadraticPenalty :: Mode a => QuadraticPenalty a -> PenaltyFn a
quadraticPenalty (QuadraticPenalty c b a) (Error x) =
  Penalty $ auto a * x * x
          + auto b * x
          + auto c


prohibitiveQuadraticPenalty :: Num a => QuadraticPenalty a
prohibitiveQuadraticPenalty = QuadraticPenalty 0 0 1000
