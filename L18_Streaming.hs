{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}

module L18_Streaming where

import Data.List.NonEmpty (NonEmpty)
import Data.Foldable (foldl')

-- data [] a = [] | a : [a]
-- 1. in-memory (modulo laziness)
-- 2. no interleaving of effects
-- full solution: later

-- simpler problem: single-pass folds

myAverage :: Fractional a => NonEmpty a -> a
myAverage xs = sum xs / fromIntegral (length xs)

myAverageSinglePass :: Fractional a => NonEmpty a -> a
myAverageSinglePass xs =
                   -- foldl' is the most efficient
  let (sm, ln) = foldl' (\(s, l) x -> (s + x, l + 1)) (0, 0 :: Integer) xs
   in sm / fromIntegral ln
-- ^ not compositional!!

data Fold0 a b = MkFold0 (b -> a -> b) b
    -- deriving Functor

runFold0 :: Foldable f => Fold0 a b -> f a -> b
runFold0 (MkFold0 f s) = foldl' f s

data Store f a where
  MkStore :: f b -> (b -> a) -> Store f a

instance Functor (Store f) where
  fmap f (MkStore x g) = MkStore x (f . g)

type Fold1 a = Store (Fold0 a)

data Fold a b = forall s. MkFold (s -> a -> s) s (s -> b)

instance Functor (Fold a) where
  fmap g (MkFold f s e) = MkFold f s (g . e)

instance Applicative (Fold i) where
  pure x = MkFold const () (const x)
  MkFold f1 s1 e1 <*> MkFold f2 s2 e2 =
    MkFold
        (\(t1, t2) i -> (f1 t1 i, f2 t2 i))
        (s1, s2)
        \(t1, t2) -> e1 t1 (e2 t2)

runFold :: Foldable f => Fold a b -> f a -> b
runFold (MkFold f s e) xs = foldr step e xs s
 where step a e' s' = e' $! f s' a

averageFold :: Fractional a => NonEmpty a -> a
averageFold = runFold $ liftA2 (/) sumF (fromIntegral <$> lenF)
 where
   sumF :: Num a => Fold a a
   sumF = MkFold (+) 0 id

   lenF :: Fold b Int
   lenF = MkFold ((succ .) . const) 0 id

data FoldM0 m a b = forall s. MkFoldM0 (s -> a -> m s) (m s) (s -> m b)

instance Functor m => Functor (FoldM0 m a) where
  fmap g (MkFoldM0 f s e) = MkFoldM0 f s (fmap g . e)

instance Applicative m => Applicative (FoldM0 m a) where
  pure x = MkFoldM0 (\x _ -> pure x) (pure ()) (\_ -> pure x)
  MkFoldM0 f1 s1 e1 <*> MkFoldM0 f2 s2 e2 =
    MkFoldM0
      (\(t1, t2) i -> liftA2 (,) (f1 t1 i) (f2 t2 i))
      (liftA2 (,) s1 s2)
      \(t1, t2) -> e1 t1 <*> e2 t2

data FoldM m a b = forall s. MkFoldM (s -> a -> m (Either s b)) (m s) (s -> m b)
-- two instances of applicative:
-- 1. as before, "zipping" folds
-- 2. when first fold finishes, second starts (corresp. pure = immediately stop)

-- Church encoding of datatypes

data Pair a b = MkPair a b

type ChurchPair a b = forall c. (a -> b -> c) -> c -- uncurry

pairChurch :: Pair a b -> ChurchPair a b
pairChurch (MkPair a b) f = f a b

churchPair :: ChurchPair a b -> Pair a b
churchPair f = f MkPair

data Either' a b = Left' a | Right' b

type ChurchEither a b = forall c. (a -> c) -> (b -> c) -> c -- either

eitherChurch :: Either' a b -> ChurchEither a b
eitherChurch (Left' a) f _ = f a
eitherChurch (Right' b) _ g = g b

churchEither :: ChurchEither a b -> Either' a b
churchEither h = h Left' Right'

-- data [] a = [] | a : [a]
type ChurchList a = forall c. c -> (a -> c -> c) -> c -- foldr

-- ListT m a = m (ListTData m a)
-- ListTData m a = Nil | Cons a (ListT m a)

newtype ChurchListT m a =
  MkChurchListT (m (forall c. c -> (a -> m c -> c) -> c))

-- unfoldr as a datatype

data Stream a = forall s. MkStream (s -> Maybe (a, s)) s

instance Foldable Stream where
  foldr f s (MkStream g x) = case g x of
    Just (a, x') -> f a $ foldr f s (MkStream g x')
    Nothing -> s

data StreamM m a = forall s. MkStreamM (s -> m (Maybe (a, s))) (m s)
-- ^ streamly def.
