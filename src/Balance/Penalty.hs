{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}


module Balance.Penalty
  ( Error (..)
  , Penalty (..)
  , PenaltyFn
  , QuadraticPenalty (..)
  , quadraticPenalty
  , prohibitiveQuadraticPenalty
  ) where


newtype Error a = Error { unError :: a }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)


newtype Penalty a = Penalty { unPenalty :: a }
  deriving (Eq, Show, Read, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)


type PenaltyFn = forall a. ( Real a, Fractional a ) => Error a -> Penalty a


data QuadraticPenalty = QuadraticPenalty
  { extraConstant        :: Double
  , linearCoefficient    :: Double
  , quadraticCoefficient :: Double }


quadraticPenalty :: QuadraticPenalty -> PenaltyFn
quadraticPenalty (QuadraticPenalty c b a) (Error x) =
  Penalty $ fromRational (toRational a) * x * x
          + fromRational (toRational b) * x
          + fromRational (toRational c)


prohibitiveQuadraticPenalty :: QuadraticPenalty
prohibitiveQuadraticPenalty = QuadraticPenalty 0 0 1000
