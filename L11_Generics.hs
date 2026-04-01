




{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module L11_Generics where

import Data.Bits (testBit)
import GHC.Generics qualified as G
import Data.Monoid (Sum)

class SayHi a where
    sayHi :: a -> String
    sayHi _ = "Hiiii!!!"

class ToBits a where
    toBits :: a -> [Bool]
    default toBits :: (G.Generic a, GToBits (G.Rep a)) => a -> [Bool]
    toBits = toBitsGeneric

instance ToBits Int where
    toBits x = [ testBit x i | i <- [0..63 :: Int] ]

-- ... more instances for builtin types ...

data IntList = IntNil | IntCons Int IntList
    deriving stock (Eq, Ord, Show, G.Generic)
    deriving SayHi via IntList'

instance ToBits IntList where
    toBits IntNil = [False]
    toBits (IntCons x xs) = [True] ++ toBits x ++ toBits xs

-- Deriving Strategies

newtype IntList' = MkIntList' IntList
    deriving stock (Eq, Ord, Show)
    deriving newtype ToBits
    deriving anyclass SayHi

toBitsGeneric :: (G.Generic a, GToBits (G.Rep a)) => a -> [Bool]
toBitsGeneric = gToBits . G.from

toBitsIntList :: IntList -> [Bool]
toBitsIntList = toBitsGeneric

class GToBits f where
    gToBits :: f a -> [Bool]

instance GToBits f => GToBits (G.M1 i m f) where
    gToBits = gToBits . G.unM1

instance (GToBits f, GToBits g) => GToBits (f G.:+: g) where
    gToBits (G.L1 f) = False : gToBits f
    gToBits (G.R1 g) = True : gToBits g

instance (GToBits f, GToBits g) => GToBits (f G.:*: g) where
    gToBits (f G.:*: g) = gToBits f ++ gToBits g

instance ToBits a => GToBits (G.K1 i a) where
    gToBits = toBits . G.unK1

instance GToBits G.U1 where
    gToBits G.U1 = []

deriving anyclass instance ToBits a => ToBits [a]

data ABC = A | B [Int] | C Int ABC deriving (G.Generic, ToBits)

-- how to add deriving to classes which are not defined by us?
-- e.g. Semigroup

-- adapted from @GenericSemigroupMonoid@
newtype GenericSemigroup a = MkGenericSemigroup { unGenericSemigroup :: a }

instance
    (G.Generic a, GSemigroup (G.Rep a)) =>
    Semigroup (GenericSemigroup a) where
  MkGenericSemigroup x <> MkGenericSemigroup y =
    MkGenericSemigroup (appendGeneric x y)

data WordCount = MkWordCount { lines :: Sum Int, chars :: Sum Int }
    deriving stock G.Generic
    deriving Semigroup via GenericSemigroup WordCount

appendGeneric :: (G.Generic a, GSemigroup (G.Rep a)) => a -> a -> a
appendGeneric x y = G.to (G.from x `gappend` G.from y)

class GSemigroup f where
    gappend :: f a -> f a -> f a

instance GSemigroup f => GSemigroup (G.M1 i m f) where
    gappend (G.M1 x) (G.M1 y) = G.M1 (gappend x y)

instance (GSemigroup f, GSemigroup g) => GSemigroup (f G.:*: g) where
    gappend (f1 G.:*: g1) (f2 G.:*: g2) = gappend f1 f2 G.:*: gappend g1 g2

instance Semigroup a => GSemigroup (G.K1 i a) where
    gappend (G.K1 x) (G.K1 y) = G.K1 (x <> y)
