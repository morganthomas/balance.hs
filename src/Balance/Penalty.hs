{-# LANGUAGE RankNTypes #-}


module Balance.Penalty
  ( Penalty
  , QuadraticPenalty (..)
  , quadraticPenalty
  , prohibitiveQuadraticPenalty
  ) where


type Penalty = forall a. ( Real a, Fractional a ) => a -> a


data QuadraticPenalty = QuadraticPenalty
  { extraConstant        :: Double
  , linearCoefficient    :: Double
  , quadraticCoefficient :: Double }


quadraticPenalty :: QuadraticPenalty -> Penalty
quadraticPenalty (QuadraticPenalty c b a) x =
  fromRational (toRational a) * x * x + fromRational (toRational b) * x + fromRational (toRational c)


prohibitiveQuadraticPenalty :: QuadraticPenalty
prohibitiveQuadraticPenalty = QuadraticPenalty 0 0 1000
