{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module L13_Typeable where

import Data.Coerce (coerce)
import Data.Monoid (Sum (..))
import GHC.Base (Word32#, Int32#, int32ToWord32#)
import Data.Data ((:~:) (Refl))
import Type.Reflection (SomeTypeRep (..), typeRep)
import Data.Dynamic (Dynamic (..), toDyn)
import Data.Map (Map, insert)
import Data.Typeable (Typeable)
import qualified Data.Map as M
import Data.Type.Equality (testEquality)

--- explicit casts --------------------------------

transform :: Int -> Float
transform = fromIntegral

transform' :: Int -> Float
transform' = error "TODO: bit reinterpretation"

constant :: Num a => a
constant = 5

--- coercions -------------------------------------

newtype MyInt = MkInt Int

coercion :: Int -> MyInt
coercion = coerce

coercion' :: MyInt -> Int
coercion' = coerce

coerce'' :: (MyInt, Float) -> (Int, Sum Float) -- import Sum (..) is crucial!
coerce'' = coerce

int2word :: Int32# -> Word32#
int2word = int32ToWord32#

-- roles ------------------------------------------

newtype Nominal a = MkNominal { runNominal :: a }

type role Nominal nominal -- representational, phantom

-- type equality ----------------------------------

example :: a ~ Bool => f a -> f Bool
example x = x

example' :: a :~: Bool -> f a -> f Bool
example' Refl x = x

-- Map from types to values of such type: Map SomeTypeRep Dynamic

newtype TypeMap = TypeMap (Map SomeTypeRep Dynamic)

get :: forall b. Typeable b => TypeMap -> Maybe b
get (TypeMap m) =
  let rep = typeRep @b
   in case M.lookup (SomeTypeRep rep) m of
       Just (Dynamic rep' x) -> case testEquality rep rep' of
         Just Refl -> Just x
         Nothing -> error "types mismatch"
       Nothing -> Nothing

put :: forall a. Typeable a => a -> TypeMap -> TypeMap
put x (TypeMap m) =
    TypeMap $ insert (SomeTypeRep (typeRep @a)) (toDyn x) m
